# Library -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)

# Read Data ---------------------------------------------------------------

df <- read.csv("data/LEOKA_1995_2019.csv", na.strings = "")

# Get data structure
str(df)

# Clean Data --------------------------------------------------------------

# Create a vector of the categorical (factor) variables.
# This will be used to filter the dataset during exploration.
factorColumns <- colnames(df)[1:11]

# Get number of unique values
uniqueCheck <- function(df){
  uniqueValues <- df %>%
    select(factorColumns) %>%
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

# Looks good. Conver to factor.
df$DATA_YEAR <- as.factor(df$DATA_YEAR)

# Update uniqueValues and move on.
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
# No 100% numeric or NA values. Convert to factor.
df$PUB_AGENCY_NAME <- as.factor(df$PUB_AGENCY_NAME)
df$PUB_AGENCY_UNIT <- as.factor(df$PUB_AGENCY_UNIT)

# Check state abbreviations; 53 are listed.
unique(df$STATE_ABBR)
# 50 states, DC, Guam, and Virgin Islands. Convert to factor.
df$STATE_ABBR <- as.factor(df$STATE_ABBR)

# Check values. If there are no issues, convert to factor.
unique(df$DIVISION_NAME)
df$DIVISION_NAME <- as.factor(df$DIVISION_NAME)
unique(df$REGION_NAME)
df$REGION_NAME <- as.factor(df$REGION_NAME)
unique(df$AGENCY_TYPE_NAME)
df$AGENCY_TYPE_NAME <- as.factor(df$AGENCY_TYPE_NAME)
df$COUNTY_NAME <- as.factor(df$COUNTY_NAME)
unique(df$POPULATION_GROUP_DESC)
df$POPULATION_GROUP_DESC <- as.factor(df$POPULATION_GROUP_DESC)

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
# Capture the real activity names in a vector, called `activities`.
# Get a list of indices of records that have real names.
# Remove all numeric values from the dataframe.
# Double-check ACTIVITY_NAME to confirm the drop.
activities <- unique(df$ACTIVITY_NAME)[1:11]
indices <- grepl(paste(activities, collapse = "|"), df$ACTIVITY_NAME)
df <- df[indices, ]

unique(df$ACTIVITY_NAME)
unique(df$ACTIVITY_ID)
df$ACTIVITY_NAME <- as.factor(df$ACTIVITY_NAME)
df$ACTIVITY_ID <- as.factor(df$ACTIVITY_ID)

# Rerun unique checker.
uniqueValues <- uniqueCheck(df)
uniqueValues

# Unique values look representative of real data.
# Check the structure; the first 11 variables should now be factors.
str(df)

# Convert the rest of the variables from character to numeric.
df <- df %>%
  mutate_if(is.character, as.numeric)

# Confirm the structure output.
str(df)

# Data now consists of 11 factor variables and 12 quantitative numeric variables.


# Exploratory -------------------------------------------------------------
##
# Group by region and get sums of numeric variables.
LEOKA <- df %>%
  group_by(REGION_NAME) %>%
  summarise_if(is.numeric, sum)

# Tally across the rows (all assaults)
totals <- rowSums(LEOKA[2:12])

# Get the percentage of cleared charges
percentCleared <- round((LEOKA[13]/totals)*100, 2)
colnames(percentCleared) <- "percentCleared"

# Combine results and view.
cbind(LEOKA[1], totals, LEOKA[13], percentCleared) %>%
  arrange(desc(percentCleared))

##
# Group by ACTIVITY_NAME, DATA_YEAR and get sums of numeric variables.
LEOKA.act <- df %>%
  group_by(ACTIVITY_NAME, DATA_YEAR) %>%
  summarise_if(is.numeric, sum)

# Tally across the rows (all assaults)
totals.act <- rowSums(LEOKA.act [3:13])

# Change DATA_YEAR from factor to number
LEOKA.act$DATA_YEAR <- year(as.Date(LEOKA.act$DATA_YEAR,
                                      format="%Y"))

# Visualization ----------------------------------------------------------

# Visualize distribution of change in activities over time
ggplot(LEOKA.act, aes(x = DATA_YEAR, y = totals.act)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  geom_smooth(method = lm, show.legend = FALSE, aes(color = "orange3")) +
  facet_wrap(LEOKA.act$ACTIVITY_NAME, ncol = 3, scales = "free") +
  labs(title = "Number of Incidents Per Year by Activity", 
       x = "Year", y = "Count") +
  theme_minimal()
