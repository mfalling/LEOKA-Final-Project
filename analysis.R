
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
write.csv(Act_Yr_Wide, "output/Act_Yr_Wide.csv")

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
colnames(Recent_Cats)

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
Recent_Cats$Population <- factor(Recent_Cats$Population)
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "under 2,500"] <- "Under 25k"
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "2,500 - 9,999"] <- "Under 25k"
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "10,000 - 24,999"] <- "Under 25k"
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "25,000 - 49,999"] <- "Over 25k"
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "50,000 - 99,999"] <- "Over 25k"
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "100,000 - 249,999"] <- "Over 25k"
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "250,000 - 499,999"] <- "Over 25k"
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "500,000 - 999,999"] <- "Over 25k"
levels(Recent_Cats$Population)[levels(Recent_Cats$Population) == "1,000,000 or over"] <- "Over 25k"



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
           faclen = 0,
           box.palette = "RdGn")

