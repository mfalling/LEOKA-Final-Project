
# Library -----------------------------------------------------------------

library(dplyr)
library(rpart)        # For decision trees
library(rpart.plot)   # For decision trees

# Read and Clean Data -----------------------------------------------------

# For Linear Regression
Act_Yr <- read.csv("output/percentageActivitiesByYear.csv")

# Create a function to pivot the Act_Yr dataframe into a wider format.
pivotWider <- function(Act_Yr){
  # Create a sequence to split each year by activity (11 total)
  Act_Seq <- seq(from = 0, to = 275, by = 11)
  # Instantiate an empty dataframe, then travel along the sequence to
  # capture each year's activity statistics in a single row. Bind these
  # rows together into a wider dataframe.
  Act_Yr_Wide <- data.frame()
  for (i in 1:length(Act_Seq)){
    start <- Act_Seq[i]+1
    end <- Act_Seq[i+1]
    if(!is.na(end)){
      row <- Act_Yr[start:end, 3]
      Act_Yr_Wide <- rbind(Act_Yr_Wide, row)
    }
  }
  # Add a year column.
  Act_Yr_Wide$Year <- 1995:2019
  # Rename the columns to match the activity types.
  colnames(Act_Yr_Wide)[1:11] <- Act_Yr[1:11, 2]
  # Move the "Year" column to the front.
  Act_Yr_Wide <- Act_Yr_Wide[, c(12, 1:11)]
  
  return(Act_Yr_Wide)
}

Act_Yr <- Act_Yr[, 2:4]
Act_Yr_Wide <- pivotWider(Act_Yr)

# Linear Regression -------------------------------------------------------

# From https://www.datacamp.com/community/tutorials/linear-regression-R

# Best Fit: `Handling Persons with Mental Illness` at p < 0.000000140 ***
#            with an Adjusted R2 of .7025
bestfit <- lm(`Handling Persons with Mental Illness` ~ Year, data = Act_Yr_Wide)
summary(bestfit)

# Extract the statistics for `Handling Persons with Mental Illness`
Mental_Ill <- Act_Yr_Wide[, c(1, 7)]

# Plot the linear regression
png(file = "LinearRegression.png")
plot(Mental_Ill, type = "l",
     main = "LEOKA increasing by 25 officers each year \ndue to Handling Persons with Mental Illness",
     sub = "P-Value for Year < 0.000000140 ***, Adjusted R2 = 0.7025",
     xlab = "Year",
     ylab = "Total LEOKA per year")
abline(bestfit, col = "red")
dev.off()


# Poor performing linear models, based on Adjusted R2

# *** but Adjusted R2 = 0.5984
badfit1 <- lm(`Robberies in Progress or Pursuing Robbery Suspects` ~ Year, data = Act_Yr_Wide)
summary(badfit1)

# *** but Adjusted R2 = 0.5853 
badfit2 <- lm(`Traffic Pursuits and Stops` ~ Year, data = Act_Yr_Wide)
summary(badfit2)

# *** but Adjusted R2 = 0.5545 
badfit3 <- lm(`Investigating Suspicious Persons or Circumstances` ~ Year, data = Act_Yr_Wide)
summary(badfit3)

# ** but Adjusted R2 = 0.276 
badfit4 <- lm(`Handling, Transporting, Custody of Prisoners` ~ Year, data = Act_Yr_Wide)
summary(badfit4)

# * but Adjusted R2 = 0.1242 
badfit5 <- lm(`Burglaries in Progress or Pursuing Burglary Suspects` ~ Year, data = Act_Yr_Wide)
summary(badfit5)



# Read and Clean Data -----------------------------------------------------

# For Decision Trees
Recent_Cats <- read.csv("output/Recent_Cats.csv")
colnames(Recent_Cats)

Recent_Cats <- Recent_Cats[, -(1:2)]
legend <- unique(Recent_Cats[, 4:5])
Recent_Cats <- Recent_Cats[, -5]


Recent_Cats$ACTIVITY_ID <- as.factor(Recent_Cats$ACTIVITY_ID)

colnames(Recent_Cats)[3] <- "Population"
Recent_Cats$Population <- gsub("Cities ", "", Recent_Cats$Population)
Recent_Cats$Population <- gsub("from ", "", Recent_Cats$Population)
Recent_Cats$Population <- gsub("thru", "-", Recent_Cats$Population)


# Decision Tree -----------------------------------------------------------

# As per pg 207, set the minimum observations to 10% of the dataset size.
observations <- nrow(Recent_Cats)*0.1
nrow(Recent_Cats)*0.10

# Run, view, and plot model.
fit <- rpart(assault ~ ., 
             data = Recent_Cats, 
             method = "class", 
             control = rpart.control(minsplit = observations),
             parms = list(split = "information"))
summary(fit)
rpart.plot(fit, 
           type = 4, 
           extra = 102, 
           clip.right.labs = FALSE, 
           varlen = 0, faclen = 0)



# Because the decision tree splits at "Under 25k" and "Over 25k", I'm changing the factors to reflect this.
dt$Population <- factor(dt$Population)
levels(dt$Population)[levels(dt$Population) == "under 2,500"] <- "Under 25k"
levels(dt$Population)[levels(dt$Population) == "2,500 - 9,999"] <- "Under 25k"
levels(dt$Population)[levels(dt$Population) == "10,000 - 24,999"] <- "Under 25k"
levels(dt$Population)[levels(dt$Population) == "25,000 - 49,999"] <- "Over 25k"
levels(dt$Population)[levels(dt$Population) == "50,000 - 99,999"] <- "Over 25k"
levels(dt$Population)[levels(dt$Population) == "100,000 - 249,999"] <- "Over 25k"
levels(dt$Population)[levels(dt$Population) == "250,000 - 499,999"] <- "Over 25k"
levels(dt$Population)[levels(dt$Population) == "500,000 - 999,999"] <- "Over 25k"
levels(dt$Population)[levels(dt$Population) == "1,000,000 or over"] <- "Over 25k"



# Rerun the model (results are the same, but labels are cleaner)
fit <- rpart(assault ~ ., 
             data = Recent_Cats, 
             method = "class", 
             control = rpart.control(minsplit = observations),
             parms = list(split = "information"))
rpart.plot(fit, 
           type = 4, 
           extra = 2, 
           clip.right.labs = FALSE, 
           varlen = 0, 
           faclen = 0)

# To Do: Make the ACTIVITY_ID more aesthetically pleasing to look at (e.g., recode)



