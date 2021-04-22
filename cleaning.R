# Library -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(factoextra)

# Read and Clean Data -----------------------------------------------------

# Read data
df <- read.csv("data/LEOKA_1995_2019.csv", na.strings = "")
str(df)



# Drop non-year values from DATA_YEAR
years <- unique(df$DATA_YEAR)[1:25]
indices <- grepl(paste(years, collapse = "|"), df$DATA_YEAR)
df <- df[indices, ]




# Drop NA values from COUNTY_NAME
indices <- is.na(df$COUNTY_NAME)
df <- df[-indices, ]




# Drop inaccurate values from ACTIVITY_NAME
activityType <- unique(df$ACTIVITY_NAME)[1:11]
indices <- grepl(paste(activityType, collapse = "|"), df$ACTIVITY_NAME)
df <- df[indices, ]




# Convert quantitative variables to numeric.
df[, 12:23] <- sapply(df[, 12:23], as.numeric)




# Filter to Cities and drop Territories.
df <- df %>%
  filter(grepl("Cities", POPULATION_GROUP_DESC)) %>%
  filter(REGION_NAME != "U.S. Territories")




# Perform data validation on assault counts.
# Add rowSums for officersAssaulted and assaultsConducted.
# Then, filter the dataset to only keep records where these numbers match.
officerType <- colnames(df)[12:18]
assaultType <- colnames(df)[19:22]

df <- df %>%
  mutate(officersAssaulted = rowSums(df[, officerType])) %>%
  mutate(assaultsConducted = rowSums(df[, assaultType])) %>%
  filter(officersAssaulted == assaultsConducted)

write.csv(df, "output/cleandf.csv")

# Create Subsets ----------------------------------------------------------
#
# Requirements generated during April 8th and April 11th Meetings.
#
#      SUBSETS:
#      1. 25 records (by year) with metadata + officer & assaults
#      2. 5 records (by region) with metadata + officer & assaults
#      3. 100 records (by region and year) with year, region, and assaults
#      4. 11 records (by activities) with ratios of one vs two officers
#      5. 275 (by year and region) Assault Ratios by Type
#      6. 225 (by year and state) Percentage of activities by year
#      7. Merged df
#
#   -----------------------------------------------------------------------

# Subset 1: Total Assaults By Year ----------------------------------------

# Get the column names for officer and assault types
meta <- colnames(df)[1:9]
activities <- colnames(df)[10:11]

# DATASET 1: Number of officers assaulted, whole set. 
totalAssaults <- df[, c(meta, activities, "officersAssaulted")]

# DATASET 2: Number of officers assaulted by year.
Aslt_Yr <- totalAssaults %>%
  group_by(DATA_YEAR) %>%
  tally(officersAssaulted, name = "officersAssaulted") %>%
  as.data.frame()
head(Aslt_Yr)
summary(Aslt_Yr)

write.csv(Aslt_Yr, "output/totalAssaultsByYear.csv")




# Subset 2: Total Assaults By Region --------------------------------------

# DATASET 3: Number of officers assaulted, by region.
Aslt_Reg <- totalAssaults %>%
  group_by(REGION_NAME) %>%
  tally(officersAssaulted, name = "officersAssaulted") %>%
  as.data.frame()
head(Aslt_Reg)
summary(Aslt_Reg)
write.csv(Aslt_Reg, "output/totalAssaultsByRegion.csv")




# Subset 3: Total Assaults By Region and Year -----------------------------

# DATASET 3.5: Bonus -- By year and region.
Aslt_Reg_Yr <- totalAssaults %>%
  group_by(DATA_YEAR, REGION_NAME) %>%
  tally(officersAssaulted, name = "officersAssaulted") %>%
  as.data.frame()
head(Aslt_Reg_Yr)
summary(Aslt_Reg_Yr)
write.csv(Aslt_Reg_Yr, "output/totalAssaultsByRegionAndYear.csv")




# Subset 4: Assault Ratios By Activity Type -------------------------------

# Get the names of all three columns that represent alone LEO
aloneCols <- officerType[c(2,4, 6)]

# Get the LEOKA sum for two officers and alone officers (by activity)
Act_Sums <- df %>%
  group_by(ACTIVITY_NAME) %>%
  summarise_at(c("TWO_OFFICER_VEHICLE_ACTUAL", 
                 aloneCols), sum) %>%
  as.data.frame()
head(Act_Sums)
Act_Sums

# For convenience, get the rowSums prior to making the ratios below.
aloneSums <- rowSums(Act_Sums[aloneCols])

# Create two sets of ratios (explained below)
Act_Ratio <- Act_Sums %>%
  mutate(partnerRatio1 = (TWO_OFFICER_VEHICLE_ACTUAL)/
           (TWO_OFFICER_VEHICLE_ACTUAL + ONE_OFFICER_ALONE_ACTUAL)) %>%
  mutate(aloneRatio1 = (ONE_OFFICER_ALONE_ACTUAL)/
           (TWO_OFFICER_VEHICLE_ACTUAL + ONE_OFFICER_ALONE_ACTUAL)) %>%
  mutate(partnerRatio2 = (TWO_OFFICER_VEHICLE_ACTUAL)/
           (TWO_OFFICER_VEHICLE_ACTUAL + aloneSums)) %>%
  mutate(aloneRatio2 = (aloneSums)/
           (TWO_OFFICER_VEHICLE_ACTUAL + aloneSums))
head(Act_Ratio)
write.csv(Act_Ratio,"output/activitiesWithRatios.csv")


# Subset 5: Percentage of Activities By Year ------------------------------

# Get the breakdown of activities per year.
totalAssaultsByYrAct <- df %>%
  group_by(DATA_YEAR, ACTIVITY_NAME) %>%
  tally(officersAssaulted)

# Join the activities per year to the "by Year" dataset
# This provides the denominator for the ratios.
Act_Yr <- inner_join(totalAssaultsByYrAct, 
                     Aslt_Yr,
                     by = "DATA_YEAR")

Act_Yr <- Act_Yr %>%
  mutate(ratio = n/officersAssaulted)

# Rename columns for ease of understanding
colnames(Act_Yr)[3:4] <- c("activityTotals", "yearlyTotals")


head(Act_Yr)

write.csv(Act_Yr, "output/percentageActivitiesByYear.csv")




# Subset 6: Assaults by State and Year ------------------------------------

Aslt_St_Yr <- df
Aslt_St_Yr$DATA_YEAR <- gsub(paste(years[1:5], collapse = "|"), "'95-'99", df$DATA_YEAR)
Aslt_St_Yr$DATA_YEAR <- gsub(paste(years[6:10], collapse = "|"), "'00-'04", Aslt_St_Yr$DATA_YEAR)
Aslt_St_Yr$DATA_YEAR <- gsub(paste(years[11:15], collapse = "|"), "'05-'09", Aslt_St_Yr$DATA_YEAR)
Aslt_St_Yr$DATA_YEAR <- gsub(paste(years[16:20], collapse = "|"), "'10-'14", Aslt_St_Yr$DATA_YEAR)
Aslt_St_Yr$DATA_YEAR <- gsub(paste(years[21:25], collapse = "|"), "'15-'19", Aslt_St_Yr$DATA_YEAR)

Aslt_St_Yr <- Aslt_St_Yr %>%
  group_by(DATA_YEAR, STATE_ABBR) %>%
  tally(officersAssaulted, name = "officersAssaulted")
head(Aslt_St_Yr)
write.csv(Aslt_St_Yr, "output/AssaultsByStateAndYear.csv")


# Subset 7: Decision Trees ------------------------------------------------

# Create categorical column based on whether a record reported a LEOKA
df$assault <- ifelse(test = df$officersAssaulted > 0, 
                     yes = "Assault", 
                     no = "No Assault")

# Select categories useful for decision tree modeling (data from last 5 years)
Recent_Cats <- df %>%
  select(DATA_YEAR, REGION_NAME, AGENCY_TYPE_NAME, POPULATION_GROUP_DESC,
         ACTIVITY_ID, ACTIVITY_NAME, assault) %>%
  filter(DATA_YEAR %in% years[21:25])
head(Recent_Cats)
write.csv(Recent_Cats, "output/Recent_Cats.csv")



#   -----------------------------------------------------------------------

# For linear regression, matching the Decision Tree specifications
# Note: This was ultimately not used.
Act_Yr_Filtered <- df %>%
  filter(POPULATION_GROUP_DESC %in% unique(df$POPULATION_GROUP_DESC)[c(3, 4, 6, 7, 8, 9)]) %>%
  filter(DATA_YEAR %in% years[21:25]) %>%
  group_by(DATA_YEAR, ACTIVITY_NAME) %>%
  tally(officersAssaulted)
head(Act_Yr_Filtered)
