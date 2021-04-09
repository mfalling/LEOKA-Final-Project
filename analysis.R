# Library -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)


# Read Data ---------------------------------------------------------------

df <- read.csv("data/LEOKA_1995_2019.csv", na.strings = "")

# Get data structure
str(df)


# Clean Data --------------------------------------------------------------

# Create a vector of the dimensions (qualitative variables)
# This will be used to filter the dataset during exploration
dimensions <- colnames(df)[1:11]

# Get number of unique values
uniqueCheck <- function(df){
  uniqueValues <- df %>%
    select(dimensions) %>%
    summarise_all(n_distinct) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    rename(variables = rowname, unique = V1)
  return(uniqueValues)
}

# View results
uniqueValues <- uniqueCheck(df)
uniqueValues

# Expecting 25 years, but column has 42 unique values. Check DATA_YEAR.
unique(df$DATA_YEAR)

# This column contains non-year values that need to be dropped.
# Capture the real years in a vector, called `years`.
# Get a list of indices of records that have real years.
# Remove all non-year values from the dataframe.
# Double-check DATA_YEAR to confirm the drop.
years <- unique(df$DATA_YEAR)[1:25]
indices <- grepl(paste(years, collapse = "|"), df$DATA_YEAR)
df <- df[indices, ]
unique(df$DATA_YEAR)

# Update uniqueValues
uniqueValues <- uniqueCheck(df)
uniqueValues

unique(df$PUB_AGENCY_NAME)
# Too many unique values to manually determine potential inaccuracies.
# Check for 100% numeric values (operating assumption: No agency name is entirely numeric)
# Then, check for NA values.
indices <- grepl(paste("\b[[:digit:]]\b", collapse = "|"), df$PUB_AGENCY_NAME)
unique(indices)
indices <- is.na(df$PUB_AGENCY_NAME)
unique(indices)
# No 100% numeric or NA values.

# Check state abbreviations; 53 are listed.
unique(df$STATE_ABBR)
# 50 states, DC, Guam, and Virgin Islands.

# Check values. If there are no issues, move on.
unique(df$DIVISION_NAME)
unique(df$REGION_NAME)
unique(df$AGENCY_TYPE_NAME)

# Rename the Possessions population group.
pop <- unique(df$POPULATION_GROUP_DESC)
df <- df %>%
  mutate(POPULATION_GROUP_DESC = 
           case_when(POPULATION_GROUP_DESC == pop[20] ~ "Possessions",
                     TRUE ~ POPULATION_GROUP_DESC))

# Check COUNTY_NAME for numeric and NA values.
indices <- grepl(paste("\b[[:digit:]]\b", collapse = "|"), df$COUNTY_NAME)
unique(indices)
indices <- is.na(df$COUNTY_NAME)
unique(indices)
# NA values exist. Drop them.
df <- df[-indices, ]

# There are too many activity IDs.
unique(df$ACTIVITY_ID)
# Check the activity names for clues on what to drop.
unique(df$ACTIVITY_NAME)

# Some of these activity names are numeric values, but they should be descriptive words.
# Capture the real activity names in a vector, called `activityType`.
# Get a list of indices of records that have real names.
# Remove all numeric values from the dataframe.
# Double-check ACTIVITY_NAME to confirm the drop.
activityType <- unique(df$ACTIVITY_NAME)[1:11]
indices <- grepl(paste(activityType, collapse = "|"), df$ACTIVITY_NAME)
df <- df[indices, ]

# Confirm activity names and IDs are updated appropriately.
unique(df$ACTIVITY_NAME)
unique(df$ACTIVITY_ID)

# Rerun unique checker.
uniqueValues <- uniqueCheck(df)
uniqueValues

# Unique values look representative of real data.
# Check structure
str(df)

# Convert quantitative variables to numeric.
df[, 12:23] <- sapply(df[, 12:23], as.numeric)
str(df)

# Confirm the structure output.
str(df)

# Data now consists of 11 character variables and 12 numeric variables

# Preliminary Exploration -------------------------------------------------

## COUNTS BY REGION
# Group by region and get sums of numeric variables.
LEOKA.reg <- df %>%
  group_by(REGION_NAME) %>%
  summarise_if(is.numeric, sum)
LEOKA.reg

# Tally across the rows (all assaults)
colnames(LEOKA.reg[2:12])
totals.reg <- rowSums(LEOKA.reg[2:12])
totals.reg

# Get the percentage of cleared charges
percentCleared <- round((LEOKA.reg[13]/totals.reg)*100, 2)
colnames(percentCleared) <- "percentCleared"

# Combine results and view.
cbind(LEOKA.reg[1], totals.reg, LEOKA.reg[13], percentCleared) %>%
  arrange(desc(percentCleared))


## COUNTS BY POPULATION
options(scipen = 999)
df %>%
  group_by(POPULATION_GROUP_DESC) %>%
  tally() %>%
  ggplot(aes(x = reorder(POPULATION_GROUP_DESC, -n), y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Total Records Per Population Group",
       x = "Population Group",
       y = "Number of Records")


##
# Group by ACTIVITY_NAME, DATA_YEAR and get sums of numeric variables.
LEOKA.act <- df %>%
  group_by(ACTIVITY_NAME, DATA_YEAR) %>%
  summarise_if(is.numeric, sum)

# Tally across the rows (all assaults)
totals.act <- rowSums(LEOKA.act [3:13])

# Change DATA_YEAR from factor to number
LEOKA.act$DATA_YEAR <- year(as.Date(LEOKA.act$DATA_YEAR,
                                    format = "%Y"))

# Visualization ----------------------------------------------------------

# Visualize distribution of change in activities over time
ggplot(LEOKA.act, aes(x = DATA_YEAR, y = totals.act)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  geom_smooth(method = lm, show.legend = FALSE, aes(color = "orange3")) +
  facet_wrap(LEOKA.act$ACTIVITY_NAME, ncol = 3, scales = "free") +
  labs(title = "Number of Incidents Per Year by Activity", 
       x = "Year", y = "Count") +
  theme_minimal()



# Visualization of Trending Activities: Scaled ----------------------------

# Using Sean's graph above... Cutting the data to the three most
# trending activities and visualizating them on the same Y scale.

# Trending activities based on the visualization above
trending <- activityType[c(1, 4, 10)]

# Filter the dataset to only trending activities in cities.
cleaned <- df %>%
  filter(grepl(paste(trending, collapse = "|"), ACTIVITY_NAME)) 

# Dataset filtered to 609229 observations
nrow(cleaned)


# Group by ACTIVITY_NAME, DATA_YEAR and get sums of numeric variables.
LEOKA.act2 <- cleaned %>%
  group_by(ACTIVITY_NAME, DATA_YEAR) %>%
  summarise_if(is.numeric, sum)

# Tally across the rows (all assaults)
totals.act2 <- rowSums(LEOKA.act2[3:13])

# Change DATA_YEAR from factor to number
LEOKA.act2$DATA_YEAR <- as.numeric(LEOKA.act2$DATA_YEAR)

# Visualize distribution of change in activities over time
ggplot(LEOKA.act2, aes(x = DATA_YEAR, y = totals.act2)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  geom_smooth(method = lm, show.legend = FALSE, aes(color = "orange3")) +
  facet_wrap(LEOKA.act2$ACTIVITY_NAME, ncol = 3, scales = "free") +
  labs(title = "Number of Incidents Per Year by Activity", 
       x = "Year", y = "Count") +
  theme_minimal() +
  ylim(0, max(totals.act2))

# For comparison -- Same scale, full activity set.
ggplot(LEOKA.act, aes(x = DATA_YEAR, y = totals.act)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  geom_smooth(method = lm, show.legend = FALSE, aes(color = "orange3")) +
  facet_wrap(LEOKA.act$ACTIVITY_NAME, ncol = 3, scales = "free") +
  labs(title = "Number of Incidents Per Year by Activity", 
       x = "Year", y = "Count") +
  theme_minimal() +
  ylim(0, max(totals.act))


# -------------------------------------------------------------------------
#
# After April 8th Meeting.
# 
# TO DO:
# 1. Subset Data: Filter to CITIES only, drop territories
# 2. Create the following...
#     NON-AGGREGATED (expecting ~3mil records):
#      - Get row sum (type of officer; LEO, det, etc)
#      - Get row sum (type of assault; firearm, etc)
#      - Ensure these numbers match up.
#     AGGREGATED
#      - By year (row sums from above)
#      - By region (row sums from above)
#
#      DATASETS:
#      1. ~2mil records with metadata + officer & assaults
#      2. 25 records (year) with metadata + officer & assaults
#      3. 5 records (region) with metadata + officer & assaults
#      4. 11 records (activities) with ratios of one vs two officers
#
#   -----------------------------------------------------------------------


# 1. Subset the data ------------------------------------------------------

# Per group discussions, subset the data:
# 1. Filter to cities only (drop MSA and Counties)
# 2. Remove U.S Territories
# checkpoint <- df
df <- df %>%
  filter(grepl("Cities", POPULATION_GROUP_DESC)) %>%
  filter(REGION_NAME != "U.S. Territories")


# 2. Create new sets ------------------------------------------------------

# Get the column names for officer and assault types
meta <- colnames(df)[1:11]
officerType <- colnames(df)[12:18]
assaultType <- colnames(df)[19:22]

# View assaults, officers, and activities
assaultType
officerType
activityType


# TASK 1: Non-Aggregated. Perform data validation on assault counts.

# Add rowSums for officersAssaulted and assaultsConducted.
# Then, filter the dataset to only keep records where these numbers match.
df <- df %>%
  mutate(officersAssaulted = rowSums(df[, officerType])) %>%
  mutate(assaultsConducted = rowSums(df[, assaultType])) %>%
  filter(officersAssaulted == assaultsConducted)
# Only 250 records were dropped (out of 2.2mil).


# TASK 2: Aggregated. Create four datasets to view different facets of the data.

# DATASET 1: Number of officers assaulted, whole set. 
totalAssaults <- df[, c(meta, "officersAssaulted")]


# DATASET 2: Number of officers assaulted, by year.
totalAssaultsByYear <- totalAssaults %>%
  group_by(DATA_YEAR) %>%
  tally(officersAssaulted)


# DATASET 3: Number of officers assaulted, by region.
totalAssaultsByRegion <- totalAssaults %>%
  group_by(REGION_NAME) %>%
  tally(officersAssaulted)


# DATASET 3.5: Bonus -- By year and region.
totalAssaultsYrRg <- totalAssaults %>%
  group_by(DATA_YEAR, REGION_NAME) %>%
  tally(officersAssaulted)
totalAssaultsYrRg

# Bonus time-series graph with regression line.
ggplot(totalAssaultsYrRg, 
       aes(x = as.numeric(DATA_YEAR), y = n, col = REGION_NAME)) +
  geom_line(linetype = "dashed") +
  geom_smooth(formula = "y ~ x", method = "loess", se = FALSE, size = 1) +
  labs(title = "LEOKA by Region over Time",
       x = "Year",
       y = "Total LEOKA") +
  theme_minimal()


# DATASET 4: Ratios of activity

# Get the names of all three columns that represent alone LEO
aloneCols <- colnames(activities)[3:5]
aloneCols

# Get the LEOKA sum for two officers and alone officers (by activity)
activities <- df %>%
  group_by(ACTIVITY_NAME) %>%
  summarise_at(c("TWO_OFFICER_VEHICLE_ACTUAL", 
                 aloneCols), sum)
activities

# For convenience, get the rowSums prior to making the ratios below.
aloneSums <- rowSums(activities[aloneCols])

# Create two sets of ratios (explained below)
activitiesWithRatios <- activities %>%
  mutate(partnerRatio1 = (TWO_OFFICER_VEHICLE_ACTUAL)/
           (TWO_OFFICER_VEHICLE_ACTUAL + ONE_OFFICER_ALONE_ACTUAL)) %>%
  mutate(aloneRatio1 = (ONE_OFFICER_ALONE_ACTUAL)/
           (TWO_OFFICER_VEHICLE_ACTUAL + ONE_OFFICER_ALONE_ACTUAL)) %>%
  mutate(partnerRatio2 = (TWO_OFFICER_VEHICLE_ACTUAL)/
           (TWO_OFFICER_VEHICLE_ACTUAL + aloneSums)) %>%
  mutate(aloneRatio2 = (aloneSums)/
           (TWO_OFFICER_VEHICLE_ACTUAL + aloneSums))

activitiesWithRatios
# Notice the % differences when looking at the columns for Ratio1 vs Ratio2:
# Ratio1 considers two columns: 
#      1. TWO_OFFICER_VEHICLE_ACTUAL
#      2. ONE_OFFICER_ALONE_ACTUAL
# Ratio2 considers four columns: 
#      1. TWO_OFFICER_VEHICLE_ACTUAL
#      2. ONE_OFFICER_ALONE_ACTUAL
#      3. DET_SPE_ASS_ALONE_ACTUAL
#      4. OTHER_ALONE_ACTUAL
# Because of this, average ratio for "alone" in Ratio2 is 6% "more dangerous".
# Are we okay this? Does this represent reality to the best of our ability,
# given the dataset that we have?


# Bonus bonus: "Percentage of activities per year"

# Get the breakdown of activities per year.
totalAssaultsByYrAct <- df %>%
  group_by(DATA_YEAR, ACTIVITY_NAME) %>%
  tally(officersAssaulted)

# Join the activities per year to the "by Year" dataset
# This provides the denominator for the ratios.
percentageActByYear <- inner_join(totalAssaultsByYrAct, 
                                  totalAssaultsByYear, 
                                  by = "DATA_YEAR")

percentageActByYear <- percentageActByYear %>%
  mutate(ratio = n.x/n.y)

# Rename columns for ease of understanding
colnames(percentageActByYear)[3:4] <- c("activityTotals", "yearlyTotals")

percentageActByYear
