library(ggplot2)
library(data.table)
library(usmap)

# First three lines are metadata
metadata <- readLines('./CollegeData.csv', n = 3)
metadata <- strsplit(metadata, ',')
fullColumnNames <- metadata[[1]]
columnNames <- metadata[[2]]
columnClasses <- metadata[[3]]

# Custom class for date formatting
setClass('myDate')
setAs('character','myDate', function(from) as.Date(from, format = '%m/%d/%Y'))
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

# Visualization of the data we have
ggplot(data = collegeData, aes(x = Enrollment, y = AveragePositivityRate, color = TestingStrategy)) +
  geom_point() +
  labs(title = 'University Enrollment Scatter Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('./output/EnrollmentScatterPlot.png')

# Map universities represented
collegesPerState <- collegeData[, list(count = length(State)), by = list(state = State)]

plot_usmap(
  data = collegesPerState,
  values = 'count',
  include = factor(collegesPerState$state),
  color = 'transparent'
) +
  scale_fill_continuous(
    low = 'orange', high = 'red', name = 'Number of Universities', label = scales::comma
  ) +
  labs(title = 'States Represented') +
  theme(plot.title = element_text(hjust = 0.5, size = 22))

ggsave('./output/UniversitiesRepresented.png')

outputModel <- function(model, fileSuffix) {
  # boxplot(model[['residuals']],main='Boxplot: Residuals',ylab='residual value')
  
  # Save the model summary
  sink(file = paste('./output/Regression_', fileSuffix, '.txt', sep = ''))
  print(summary(model))
  sink()
  
  # Get coefficient data from our model
  coefficients <- coef(summary(model))
  coefficients <- as.data.frame(coefficients)
  colnames(coefficients) <- c('Estimate', 'StdError', 't', 'p')
  
  # Remove the Intercept row
  rowDrops <- c('(Intercept)')
  coefficients <- coefficients[!(rownames(coefficients) %in% rowDrops), ]
  
  # Visualization for coefficient confidence interval
  ggplot(data = coefficients, aes(x = rownames(coefficients), 
                                  ymin = Estimate - 2 * StdError, 
                                  ymax = Estimate + 2 * StdError, 
                                  color = p)) +
    geom_hline(yintercept = 0) +
    geom_errorbar() + 
    coord_flip() +
    labs(title = 'Regression Coefficient 95% Confidence Intervals',
         x = 'Coefficient',
         y = '95% Confidence Interval'
    ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste('./output/ConfidenceIntervals_', fileSuffix, '.png', sep = ''))
}

# Run regression

model <- lm(
  AveragePositivityRate ~
  # + Funding
  + Enrollment
  # + Region
  + StartDate
  + TestingStrategy 
  # + TestOnContactTraced 
  + HasAsymptomaticTesting 
  + PreArrivalTesting
  + UniversityAssistedQuarantine
  + PreArrivalQuarantine
  + DailyHealthChecks
  + HasInPersonClasses
  # + EventPersonLimit
  # + EventApproval
  # + LibraryOpen
  # + FansAtSportingEvents
  + PartyScene
  , 
  data = collegeData)

outputModel(model, 'WithTestingStrategy')

model <- lm(
  AveragePositivityRate ~
    # + Funding
    + Enrollment
  # + Region
  + StartDate
  # + TestingStrategy 
  # + TestOnContactTraced 
  + HasAsymptomaticTesting 
  + PreArrivalTesting
  + UniversityAssistedQuarantine
  + PreArrivalQuarantine
  + DailyHealthChecks
  + HasInPersonClasses
  # + EventPersonLimit
  # + EventApproval
  # + LibraryOpen
  # + FansAtSportingEvents
  + PartyScene
  , 
  data = collegeData)

outputModel(model, 'WithoutTestingStrategy')





