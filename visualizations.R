
# Set Working Directory ---------------------------------------------------
setwd("C:/data/Final_Project")

# Options -----------------------------------------------------------------
options(warn=-1)

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(maps)
library(ggpmisc)



# Load Data ---------------------------------------------------------------
### Source: https://github.com/mfalling/LEOKA-Final-Project/tree/main/output

df1.Act_Yr <- read.csv("percentageActivitiesByYear.csv", na.strings = "")
df2.Act_Ratio <- read.csv("activitiesWithRatios.csv", na.strings = "")
df3.Aslt_St_Yr <- read.csv("AssaultsByStateAndYear.csv", na.strings = "")
df4.Aslt_Reg <- read.csv("totalAssaultsByRegion.csv", na.strings = "")
df5.Aslt_Reg_Yr <- read.csv("totalAssaultsByRegionAndYear.csv", na.strings = "")
df6.Aslt_Yr <- read.csv("totalAssaultsByYear.csv", na.strings = "")
df7.US_Pop <- read.csv("nst-est2019-01.csv", na.strings = "", 
                       header=TRUE, stringsAsFactors=FALSE)

# Prepare Data --------------------------------------------------------------

### df1.Act_Yr ###
# Combine Burglaries and Robberies into one group
df1.Act_Yr$ACTIVITY_NAME <- gsub("Burglaries i", "I", df1.Act_Yr$ACTIVITY_NAME)
df1.Act_Yr$ACTIVITY_NAME <- gsub("Robberies i", "I", df1.Act_Yr$ACTIVITY_NAME)
df1.Act_Yr$ACTIVITY_NAME <- gsub("Burglary ", "", df1.Act_Yr$ACTIVITY_NAME)
df1.Act_Yr$ACTIVITY_NAME <- gsub("Robbery ", "", df1.Act_Yr$ACTIVITY_NAME)

# Combine Ambush and Civil Disorder into "all other"
df1.Act_Yr$ACTIVITY_NAME <- gsub(
  "Ambush - No Warning", "All Other", df1.Act_Yr$ACTIVITY_NAME)
df1.Act_Yr$ACTIVITY_NAME <- gsub(
  "Civil Disorder", "All Other", df1.Act_Yr$ACTIVITY_NAME)

# Segment for bar graphs
# factor levels
df1.flevels <- c('Responding to Disturbance Calls', 
             'Attempting Other Arrests',
             'Handling, Transporting, Custody of Prisoners', 
             'Investigating Suspicious Persons or Circumstances', 
             'Traffic Pursuits and Stops',
             'Handling Persons with Mental Illness',
             'In Progress or Pursuing Suspects',
             'All Other')

df1.Act_Yr$ACTIVITY_NAME <- as.factor(df1.Act_Yr$ACTIVITY_NAME)
df1.Act_Yr$ACTIVITY_NAME <- ordered(df1.Act_Yr$ACTIVITY_NAME, 
                                    levels = df1.flevels)

df1.Act_Yr <- df1.Act_Yr[,c(-1,-5)] #Remove unused col

df1.Act_Yr <- df1.Act_Yr %>%  #Group and summarize
  group_by(ACTIVITY_NAME, DATA_YEAR) %>%
  summarise_if(is.numeric, sum)



df1.Act_Yr_bg <- df1.Act_Yr[,c(-1,-4)] # Background Data - full
df1.Act_Yr_ss <- subset(df1.Act_Yr, DATA_YEAR == "2019") #subset for value label

### df2.Act_Ratio ###
# factor levels (includes 2 categories lumped into "all other" from df1 and 
#                two combined)
df2.flevels <- c('Responding to Disturbance Calls', 
              'Attempting Other Arrests',
              'Handling, Transporting, Custody of Prisoners', 
              'Investigating Suspicious Persons or Circumstances', 
              'Traffic Pursuits and Stops',
              'Handling Persons with Mental Illness',
              'Burglaries in Progress or Pursuing Burglary Suspects', #added
              'Robberies in Progress or Pursuing Robbery Suspects', #added
              'Ambush - No Warning', #added
              'Civil Disorder', #added
              'All Other')

df2.Act_Ratio$ACTIVITY_NAME <- as.factor(df2.Act_Ratio$ACTIVITY_NAME)
df2.Act_Ratio$ACTIVITY_NAME <- ordered(df2.Act_Ratio$ACTIVITY_NAME, 
                                       levels = df2.flevels)

### df3.Aslt_St_Yr ###

df3.Aslt_St_Yr <- df3.Aslt_St_Yr[,c(-1)]  #Remove unused col
df3.Aslt_St_Yr$STATE_ABBR <- gsub("NB", "NE", df3.Aslt_St_Yr$STATE_ABBR)
  #Nebraska's changed abbreviation from NB to NE in 1969

df3.Aslt_St_Yr$region <-              #Convert abbreviations to names/lower case
  tolower(state.name[match(df3.Aslt_St_Yr$STATE_ABBR,state.abb)])

df3.Aslt_St_2015_to_2019 <- df3.Aslt_St_Yr %>%    #Group, filter, and summarize
  filter(DATA_YEAR == "'15-'19") %>% #Select only 15-19
  group_by(STATE_ABBR, region) %>%
  summarise_if(is.numeric, sum)

df3.Aslt_St_2015_to_2019$officersAssaultedAverage <- #create 5 year average
  df3.Aslt_St_2015_to_2019$officersAssaulted / 5

### df5.Aslt_Reg_Yr ###
names(df5.Aslt_Reg_Yr)[names(df5.Aslt_Reg_Yr) == "REGION_NAME"] <- "region"
df5.Aslt_Reg_Yr$region <- tolower(df5.Aslt_Reg_Yr$region)

### df7.US_Pop ###
df7.US_Pop$region <- tolower(df7.US_Pop$region) #lowecase
df7.US_Pop$Five_Year_Avg_Pop <- rowMeans(df7.US_Pop[,c(9:13)], na.rm=TRUE)
df7.US_Pop <- df7.US_Pop[,c(-2:-13)] #Remove unused col

### Join & cleaned Datasets ###

MainStates <- map_data("state")
MergedStates <- inner_join(df3.Aslt_St_2015_to_2019, MainStates, by = "region")
MergedStates <- inner_join(MergedStates, df7.US_Pop, by = "region")
MergedRegions <-inner_join(df5.Aslt_Reg_Yr, df7.US_Pop, by = "region")

# Visualizations -----------------------------------------------------------

### Comparison ###

# Total vs Activity Type; Col chart
# df1.Act_Yr (percentageActivitiesByYear.csv)

v1 <-
ggplot(data = df1.Act_Yr, aes(x = DATA_YEAR, y = activityTotals)) +
  geom_bar(data = df1.Act_Yr_bg, stat = "identity",
           fill = "grey", alpha = .5, aes(y = activityTotals)) +
  geom_bar(stat = "identity", color = "white", size = 0.1,
           aes(y = activityTotals, fill = ACTIVITY_NAME)) +
  facet_wrap(~ factor(ACTIVITY_NAME, levels = df1.flevels),
        ncol = 3, scales = "free") +
  geom_text(data = df1.Act_Yr_ss, vjust=-2, hjust=1,
            aes(label = sprintf("2019: %1.2f%%", 100*ratio))) +
  labs(title = "LEOKA Incidents Per Year by Activity",
       x = "Year", y = "Count") +
  theme_solarized() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0),
        strip.text.x = element_text(size = 8)) +
  scale_fill_brewer(palette = "Set2")

# Proportion stacked

v2 <- 
ggplot(data = df1.Act_Yr, 
       aes(x = DATA_YEAR, y = activityTotals, fill = ACTIVITY_NAME)) +
  geom_bar(position="fill", stat="identity", width=1, color = "white") +
  labs(title = "Proportion of LEOKA Incidents Per Year by Activity",
       x = "Year", y = "") +
  theme_solarized() +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0), legend.title = element_blank())

# Activities Change over time; % change facets
# df1.Act_Yr (percentageActivitiesByYear.csv)
my.formula <- y ~ x
v3 <-
ggplot(data = df1.Act_Yr, aes(x = DATA_YEAR, y = activityTotals, )) +
  geom_bar(stat = "identity", color = "white", size = 0.1, alpha = 0.5,
           aes(y = activityTotals)) +
  geom_smooth(method = lm, size = 1.5, show.legend = FALSE, 
              aes(color = "orange3")) +
  facet_wrap(~ factor(ACTIVITY_NAME, levels = df1.flevels),
             ncol = 3, scales = "free") +
  labs(title = "Change in Number of LEOKA Incidents",
       subtitle = "Per Year by Activity",
       x = "Year", y = "Count") +
  stat_poly_eq(formula = my.formula, color = "blue", size = 2.5,
               label.x = "right", label.y = 1,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  theme_solarized() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0))

# Number of Officers; 100% stacked chart
# df2.Act_Ratio (activitiesWithRatios.csv)

# Using Ratio1
v4 <-
ggplot(data = df2.Act_Ratio, aes(x = ACTIVITY_NAME)) +
  geom_bar(stat = "identity", aes(y = aloneRatio1*-1, fill = "Alone")) +
  geom_bar(stat = "identity", aes(y = partnerRatio1, fill = "With Partner")) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-0.6, 0.6, 0.2), 
                     labels = paste0(as.character(
                       c("60","40","20","0","20","40","60")), "%")) +
  labs(title = "Officer Activity: Alone vs. With Partner",
       subtitle = "Ratio 1",
       x = "", y = "Percentage") +
  theme_solarized() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Paired")

# Using Ratio2
v5 <-
ggplot(data = df2.Act_Ratio, aes(x = ACTIVITY_NAME)) +
  geom_bar(stat = "identity", aes(y = aloneRatio2*-1, fill = "Alone")) +
  geom_bar(stat = "identity", aes(y = partnerRatio2, fill = "With Partner")) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-0.6, 0.6, 0.2), 
                     labels = paste0(as.character(
                       c("60","40","20","0","20","40","60")), "%")) +
  labs(title = "Officer Activity: Alone vs. With Partner",
       subtitle = "Ratio 2",
       x = "", y = "Percentage") +
  theme_solarized() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2")

### Geo-spatial ###

# Regional Comparison; Col chart OR R Maps
# df4.Aslt_Reg (totalAssaultsByRegion.csv) 
# df5.Aslt_Reg_Yr (totalAssaultsByRegionAndYear.csv)

# State Comparison; Brewer heat map, R Maps
# df3.Aslt_St_Yr (AssaultsByStateAndYear.csv)
v6 <-
ggplot() + 
  geom_polygon(data=MergedStates, aes(x=long, y=lat, group=group,
                    fill = officersAssaulted/Five_Year_Avg_Pop * 100000), 
                color="white", size = 0.2)  +
  labs(title = "Average LEOKA Per Year: 2015 - 2019", 
       subtitle = "Per 100,000 People") +
  scale_fill_continuous(name="", 
                        low = "lightblue", high = "darkred",
                        na.value = "grey50") +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "") +
  scale_y_discrete(labels = NULL, breaks = NULL) + labs(y = "") +
  theme_solarized() +
  coord_fixed()

v7 <- 
  ggplot(MergedRegions,
         aes(x = as.numeric(DATA_YEAR), 
             y = officersAssaulted/Five_Year_Avg_Pop * 100000,
             col = region)) +
  geom_line(linetype = "dashed") +
  geom_smooth(formula = "y ~ x", method = "loess",
              se = FALSE, size = 1) +
  labs(title = "LEOKA by Region over Time", subtitle = "Per 100,000 People",
       x = "Year", y = "Count", colour="Region") +
  theme_solarized()

# Save & Compile ----------------------------------------------------------
plots.list = list(v1,v2,v3,v4,v5,v6,v7)
pdf("Visualizations.pdf", paper = "a4r", width = 11.349, height = 8.051)
print(plots.list)
dev.off()
