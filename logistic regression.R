rm(list = ls())

library(infotheo)
library(dplyr)
library(ggplot2)

input_data = read.csv('archive/employee_attrition_train.csv', header = T)

### EDA
input_data %>% ggplot(aes(x = Attrition, y = Age)) + geom_boxplot()
input_data %>% ggplot(aes(x = Attrition, y = MonthlyIncome)) + geom_boxplot()
input_data %>% ggplot(aes(x = Attrition, y = YearsInCurrentRole)) + geom_boxplot()
input_data %>% ggplot(aes(x = Attrition, y = YearsWithCurrManager)) + geom_boxplot()

input_data$Over18 = as.factor(input_data$Over18)
summary(input_data$Over18)

input_data$EmployeeCount = as.factor(input_data$EmployeeCount)
summary(input_data$EmployeeCount)

input_data$StandardHours = as.factor(input_data$StandardHours)
summary(input_data$StandardHours)

input_data$JobRole = as.factor(input_data$JobRole)
summary(input_data$JobRole)

categorical_var = c('BusinessTravel', 'Department', 'Education', 'EducationField',
                    'Gender', 'JobInvolvement', 'JobLevel', 'JobRole', 'JobSatisfaction',
                    'MaritalStatus', 'OverTime', 'PerformanceRating', 'StockOptionLevel',
                    'RelationshipSatisfaction', 'WorkLifeBalance')



X <- input_data[,-2]
Y <- input_data[,2]

logIncome = log(input_data$MonthlyIncome)
X$MonthlyIncome = logIncome

categorical_var_indices = c(2, 4, 6, 7, 10, 11, 13, 14, 15, 16, 17, 22, 24, 
                            25, 27, 30)

mi = data.frame(var = character(), score = double())

for (i in 1:34)
{
  new_data = data.frame(var = names(X)[i], score = mutinformation(X[,i], Y))
  mi = rbind(mi, new_data)
}

### Categorical data 

n = length(categorical_var)

high_corr = data.frame(var1 = character(),
                  var2 = character(),
                  p_value = double())
for (i in 1:n)
{
  for (j in i+1:n)
  {
    a = categorical_var_indices[i]
    b = categorical_var_indices[j]
    chi <- chisq.test(table(X[,c(a, b)]))
    if(chi$p.value < 0.05)
    {
      new_data = data.frame(var1 = names(X)[a],
                            var2 = names(X)[b],
                            p_value = chi$p.value)
      high_corr = rbind(high_corr, new_data)
    }
   }
}

### numerical correlation

var_count = dim(X)[2]

for (i in seq(34))
{
  for (j in seq(i+1,34))
  {
    if(j > 34)
    {
      next
    }
    if ((i %in% categorical_var_indices) | (j %in% categorical_var_indices))
    {
      next
    }
    X[,i] = as.numeric(X[,i])
    X[is.na(X[,i]),i] <- mean(X[,i], na.rm = T)
    X[,j] = as.numeric(X[,j])
    X[is.na(X[,j]),j] <- mean(X[,j], na.rm = T)
    pearson_corr <- cor.test(X[,i], X[,j], alternative = "two.sided")
    if(!is.na(pearson_corr$p.value))
    {
      if(pearson_corr$p.value < 0.01)
      {
        new_data = data.frame(var1 = names(X)[i],
                              var2 = names(X)[j],
                              p_value = pearson_corr$p.value)
        high_corr = rbind(high_corr, new_data)
      }
    }
  }
}

for (i in categorical_var_indices){
  X[,i] = as.factor(X[,i])
}
for (i in seq(dim(X)[2])){
  if(!i %in% categorical_var_indices)
  {
    X[,i] = as.numeric(X[,i])
    X[is.na(X[,i]),i] <- median(X[,i], na.rm = T)
  }
    
}


processed_data = cbind(X,Y)

processed_data$Y = as.factor(processed_data$Y)

glm.fit = glm('Y ~ . - Over18 - StandardHours - EmployeeCount', data = processed_data, family = "binomial")
glm.probs <- predict(glm.fit,type = "response")

glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")

table(glm.pred, Y)

cleaned_data = processed_data[,-c(3, 8, 9, 11, 19, 21, 24, 26)]
# removed over18, empoyee count, employee number, standard hours, 
# gender, performance rating -> increased yes prediction
# removed daily rate and monthly rate - box plots

glm.fit = glm('Y ~ .', data = cleaned_data, family = "binomial")
glm.probs <- predict(glm.fit, type = "response")

glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")

table(glm.pred, Y)
# variables to be dropped 
##  Over18, Employee Count, Standard Hours - only one value.
##  Employee number can also be dropped - It looks like it is nothing but employee id
##  hourly rate, daily rate, or monthly rate - Master variable monthly income can be used instead
##  Job Role is nothing but combination of Department and Job level. To me it looks like redundant variable. 

# variables to be log transformed - 
##   Monthly income - operating with dollar value. best scaling strategy is log transformation

# Variable to be scaled
##   Age, Rate, DistancefromHome, EmployeeNumber, PercentageHike, 
##   TotalWorkingHours, YearsAtCompany, YearsInCurrentRole