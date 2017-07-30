# Capstone-Project
Capstone project for the Foundations of Data Science course in Springboard

#### Load libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(scales)

#### Load in the two data tables from the DOT
fatality_monthly <- read.csv(file = "E:\\Springboard\\Datasets\\Capstone\\fatality_monthly.csv")
View(fatality_monthly)
injury_monthly <- read.csv(file = "E:\\Springboard\\Datasets\\Capstone\\injury_monthly.csv")
View(injury_monthly)

#### assign to an editable version to keep the data integrity... and so I don't mess anything up
edit_fatality_monthly <- fatality_monthly
edit_injury_monthly <- injury_monthly

#### add in a day column as a string
edit_fatality_monthly$Day <- 1L
edit_injury_monthly$Day <- 1L

#### convert the month and year columns to a factor in order to unite them
edit_fatality_monthly$MN <- as.factor(edit_fatality_monthly$MN)
edit_fatality_monthly$YR <- as.factor(edit_fatality_monthly$YR)

#### unite the columns, assign to "FullDate" column header, and write into the dataframe
edit_fatality_monthly <- unite(edit_fatality_monthly, "FullDate", c("YR", "MN", "Day"), sep = "-")
edit_injury_monthly <- unite(edit_injury_monthly, "FullDate", c("YR", "MN", "Day"), sep = "-")

#### Assign this column as a date just using the base package
edit_fatality_monthly$FullDate <- as.Date(edit_fatality_monthly$FullDate, format = "%Y-%m-%d")
edit_injury_monthly$FullDate <- as.Date(edit_injury_monthly$FullDate, format = "%Y-%m-%d")
View(edit_fatality_monthly)
##### If sorted in date order, you find that 14 lines of this data have no month and year assigned
##### Do I want to keep the month column in order to group easier later?

#### plot the data
ggplot(edit_fatality_monthly, aes(x = FullDate, y = Fatalities)) +
      geom_point()
ggplot(edit_injury_monthly, aes(x = FullDate, y = Injuries)) +
  geom_point()

#### Check for and remove any columns that do not have a date associated with them
sum(is.na(edit_fatality_monthly))
edit_fatality_monthly <- edit_fatality_monthly[complete.cases(edit_fatality_monthly$FullDate), ]
sum(is.na(edit_injury_monthly))

#### Create a unique identifier for each data point including the date and the NodeID
#### But are there any duplicates between the NodeID and the date?
#### There could have been more than one accident at a location within a month...
#### Create the potentially unique identifier and check for any duplicates in both data sources
edit_fatality_monthly$Identifier <- 
  paste(edit_fatality_monthly$FullDate, edit_fatality_monthly$NODEID, sep = "-")
length(unique(edit_fatality_monthly$Identifier)) == nrow(edit_fatality_monthly)
edit_injury_monthly$Identifier <- 
  paste(edit_injury_monthly$FullDate, edit_injury_monthly$NODEID, sep = "-")
length(unique(edit_injury_monthly$Identifier)) == nrow(edit_injury_monthly)
##### Because both of those are true, we know there are no duplicates in the identifier
##### confirming it is in fact unique

#### Now we can do a full outer join on the two data frames on the Identifier
nrow(edit_fatality_monthly) + nrow(edit_injury_monthly)
accident_monthly <- full_join(edit_fatality_monthly, edit_injury_monthly,
                              by = c("Identifier" = "Identifier"))
##### To see the number of rows that crossed over between the two dataframes:
(nrow(edit_fatality_monthly) + nrow(edit_injury_monthly)) - nrow(accident_monthly)
View(accident_monthly)

#### If we assume that any fatalities are included in the injury count for those that cross over,
#### we can get a total count of people affected by using the injury count and filling in
#### any nulls with the fatality count
accident_monthly$PeopleAffected <- accident_monthly$Injuries
accident_monthly$PeopleAffected[is.na(accident_monthly$PeopleAffected)] <-
  accident_monthly$Fatalities[is.na(accident_monthly$PeopleAffected)]




my_color = "#3D6677"
ggplot(accident_monthly, aes(x = FullDate.x, y = PeopleAffected)) +
  geom_point(size = 2, alpha = 0.5, col = my_color)

ggplot(accident_monthly, aes(x = FUllDate.x, y = PeopleAffected)) +
  stat_summary(fun.y = sum, geom = "bar")
