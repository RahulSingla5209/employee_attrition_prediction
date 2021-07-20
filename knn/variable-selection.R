attrition_data <- read.csv('Employee Attrition Clean.csv')

model <- glm(Attrition ~ Age, data=attrition_data, family='binomial')

#attrition_data$Attrition <- as.factor(attrition_data$Attrition)
#attrition_data[attrition_data==''] <- NA
#attrition_data <- na.omit(attrition_data)

full_model <- glm(Attrition ~ Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data=attrition_data, family='binomial')
sig_model <- glm(Attrition ~ BusinessTravel + EnvironmentSatisfaction + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime, data=attrition_data, family='binomial')