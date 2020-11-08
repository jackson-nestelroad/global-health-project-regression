library(ggplot2)

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

# Transform data as needed
collegeData$TestingStrategy <- apply(collegeData[c('TestingStrategy')], 2, function(strategy) strategy * 10)

# Create model
model <- lm(AveragePositivityRate ~ Enrollment + StartDate + TestingStrategy + DailyHealthChecks + PartyScene, data = collegeData)
summary(model)$coefficient

# Create visualization
ggplot(data = collegeData, aes(x = factor(TestingStrategy), y = AveragePositivityRate, color = factor(PartyScene))) +
  geom_point()

# Boxplot visualization for t value +- std. error ?