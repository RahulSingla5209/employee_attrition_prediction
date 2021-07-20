library(class) ## a library with lots of classification tools
library(kknn) ## knn library
library(caret)
library(e1071)

attrition_full_data <- read.csv('full-attrition-data.csv')
attrition_full_data[attrition_full_data == ''] <- NA

attrition_data <- na.omit(attrition_full_data)
attrition_data <- attrition_data[c('Attrition', 'BusinessTravel', 'EnvironmentSatisfaction', 
                                   'JobSatisfaction', 'MaritalStatus', 'NumCompaniesWorked', 
                                   'OverTime')]

#k-fold cross validation, number = k-folds
trControl <- trainControl(method  = "cv", number  = 10)

#knn model
fit <- train(Attrition ~.,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:100),
             trControl  = trControl,
             metric     = "Accuracy",
             data       =  attrition_data)

#get accuracy values and their k_values
accuracy <- fit$results$Accuracy
k_values <- fit$results$k

#plot accuracy
par(mfrow=c(1,1))
plot(k_values,accuracy,type="l",ylab="Accuracy",col=4,lwd=2,main="Employee Attrition (knn)",xlab="K")

#find best k
best_k <- fit$results[which.max(fit$results$Accuracy), 'k']
