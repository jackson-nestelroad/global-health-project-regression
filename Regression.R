library(ggplot2)
library(data.table)

# First three lines are metadata
metadata <- readLines('./CollegeData.csv', n = 3)
metadata <- strsplit(metadata, ',')
fullColumnNames <- metadata[[1]]
columnNames <- metadata[[2]]
columnClasses <- metadata[[3]]

# Custom class for date formatting
setClass('myDate')
setAs('character','myDate', function(from) as.Date(from, format = "%m/%d/%Y"))
columnClasses[columnClasses == 'Date'] <- 'myDate'

# Read data
collegeData <- read.csv('./CollegeData.csv', header = FALSE, skip = 3, colClasses = columnClasses)
colnames(collegeData) <- columnNames
collegeData <- as.data.table(collegeData)

gradeToNumber <- function(grade) {
  return(
    switch(grade,
           'A+' = 12,
           'A' = 11,
           'A-' = 10,
           'B+' = 9,
           'B' = 8,
           'B-' = 7,
           'C+' = 6,
           'C' = 5,
           'C-' = 4,
           'D+' = 3,
           'D' = 2,
           'D-' = 1,
           'F' = 0,
           NA
    ))
}

# Transform data as needed
collegeData <- collegeData[Ignore != 'yes' & !is.na(AveragePositivityRate)]
collegeData <- collegeData[, PartyScene := sapply(PartyScene, gradeToNumber)]

# Create model
model <- lm(
  AveragePositivityRate ~
    # Funding
    # + Enrollment 
    # + StartDate
    + TestingStrategy 
    # + TestOnContactTraced 
    + HasAsymptomaticTesting 
    + PreArrivalTesting
    + UniversityAssistedQuarantine
    # + PreArrivalQuarantine
    + DailyHealthChecks
    + HasInPersonClasses
    # + EventPersonLimit
    + EventApproval
    # + LibraryOpen
    # + FansAtSportingEvents
    + PartyScene
  , 
  data = collegeData)
summary(model)

# Create visualization
ggplot(data = collegeData, aes(x = factor(PreArrivalQuarantine), y = AveragePositivityRate)) +
  geom_point()

confidenceIntervals <- confint(model)

# boxplot(model[['residuals']],main='Boxplot: Residuals',ylab='residual value')