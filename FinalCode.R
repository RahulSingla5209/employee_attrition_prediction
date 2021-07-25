setwd("C:\\Users\\ashis\\OneDrive\\Desktop\\Resources\\Intro to ML")

##loading libraries
library(rmarkdown)
library(tinytex)
library(mice)
library(Rcpp)
library(tree)
library(randomForest)
library(gbm)
library(xgboost)
library(ROSE)
library(rpart)
library(caret)
library(dplyr)
library(ggplot2)
library(caret)
library(readr)
library(tidyr)
library(Ckmeans.1d.dp)

##load the datsets
train <- read.csv("Proj - Carlos Group 1\\archive\\WA_Fn-UseC_-HR-Employee-Attrition.csv", na.strings=c("","NA"))

##converting characters to factors for analysis and checking for null values
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], 
                                             as.factor)
count <- as.data.frame(colSums(is.na(train)))
count1 <- count[count$`colSums(is.na(train))`>0,]

##data cleaning

##removal of variables based on intution & values (constant throughout the data like employee count = 1, standard hours = 80, over18 is Y for every employee or extremely random as employee id)
## Also renaming of age variable
train1 <- train[,- which(names(train) %in% c("EmployeeCount", "StandardHours"))]
train1$Over18 <- NULL
train1$Age <- train1$ï..Age
train1$ï..Age <- NULL
train1$EmployeeNumber <- NULL

##Addition of variables based on EDA and analysis in Excel

#Hours worked per week, Average tenure of employee at previous companies, above tenure means whether an employee is above his avg tenure in current company
train1$Hoursperweek <- train1$MonthlyRate/train1$HourlyRate/7
train1$avgtenure <- train1$TotalWorkingYears/(train1$NumCompaniesWorked+1)
train1$abovetenure <- 0
for(i in 1:nrow(train1)){
  if(train1$avgtenure[i] < train1$YearsAtCompany[i]){
    train1$abovetenure[i] <- 1
  }
}
train1$abovetenure <- as.factor(train1$abovetenure)


##correlation with attrition - Chi for categorical variables and Anova for numerical variables
name <- NULL
cor <- NULL
for(i in 1:(ncol(train1)-1)){
  name[i] = colnames(train1)[i]
  if(is.factor(train1[,i])){
    cor[i] = chisq.test(train1[,i], train1$Attrition)[3]
  }
  else{
    cor[i] = summary(aov(train1[,i] ~ train1$Attrition))[[1]][["Pr(>F)"]][1]
  }
}
chi <- as.data.frame(cbind(name,cor))

chi$imp <- 0
chi$imp[chi$cor < 10^-3] <- 1
chi$imp[2] <- 0

##copying contents to another data frame
train2 <- train1[,colnames(train1) %in% chi$name[chi$imp == 1]]

##separating attrition for analysis
y <- train1$Attrition

##visualizing the data
boxplot(train2$Age ~ y)
boxplot(train2$MonthlyIncome ~ y)
boxplot(train2$TotalWorkingYears ~ y)
boxplot(train2$YearsAtCompany ~ y)
boxplot(train2$YearsInCurrentRole ~ y)
boxplot(train2$YearsWithCurrManager ~ y)
boxplot(train2$avgtenure ~ y)
boxplot(train1$PercentSalaryHike ~ y)

table(train1$Attrition, train1$BusinessTravel)
table(train1$Attrition, train1$JobInvolvement)
table(train1$Attrition, train1$JobLevel)
table(train1$Attrition, train1$JobRole)
table(train1$Attrition, train1$MaritalStatus)
table(train1$Attrition, train1$OverTime)
table(train1$Attrition, train1$StockOptionLevel)

##in case missingvalues imputation via multiple chained equations
train3 <- mice(train1, m=5, maxit = 10)
train4 <- complete(train3, 1)

##VariableImportance:
rfimp <- randomForest(Attrition ~ ., data=train4, ntree=1000,importance=TRUE)
rfImp(rfimp)
s <- as.numeric(importance(rfimp, type=2))
x <- as.data.frame(cbind(colnames(train4)[c(2:34)],s))
x$s <- as.numeric(x$s)
x <- x[order(x$s, decreasing = T),]

##Modelling

set.seed(123)

#Dividing in train and test set
sample <- sample.int(n = nrow(train4), size = floor(0.8*nrow(train4)), replace = F)
trtrain <- train4[sample, ]
trtest  <- train4[-sample, ]

#Oversampling as imbalanced data
trtrain2 <- ovun.sample(Attrition ~ ., data = trtrain, method = "over", p = 0.4)$data

#Data for modelling using subset of features
trtrain3 <- trtrain2[, c('Attrition', x$V1[1:20])]
trtest3 <- trtest[,c('Attrition', x$V1[1:20])]

#Trees - All variables, just to understand lower bound of accuracy
train_control1 = rpart.control(method = "cv", number = 10, repeats = 5)
set.seed(123)
treemodel1 <- rpart(Attrition ~ .,data = trtrain2, method = "class", control = train_control1)

predict1 <- predict(treemodel1, trtest, type = "class")
cm <- table(trtest$Attrition, predict1)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#Trees - Subset of variables
train_control2 = rpart.control(method = "repeatedcv", number = 10, repeats = 5)
set.seed(123)
treemodel2 <- rpart(Attrition ~ .,data = trtrain3, method = "class", control = train_control2)

predict2 <- predict(treemodel2, trtest3, type = "class")
cm <- table(trtest3$Attrition, predict2)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#randomforest - all variables
train_Control3 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid <- expand.grid(.mtry = c(1:36), .ntree = c(100,500, 1000))
set.seed(123)
rfmodel1 <- randomForest(Attrition ~ .,data = trtrain2, method = "class", control = train_control3, tuneGrid = tunegrid, classwt = c("No" = 5, "Yes" = 1))

predict3 <- predict(rfmodel1, trtest, type = "class")
cm <- table(trtest$Attrition, predict3)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#randomforest - subset of variables
train_Control4 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid <- expand.grid(.mtry = c(1:36), .ntree = c(100,500,1000))
set.seed(123)
rfmodel2 <- randomForest(Attrition ~ .,data = trtrain3, method = "class", control = train_control4, tuneGrid = tunegrid, classwt = c("No" = 5, "Yes" = 1))

predict4 <- predict(rfmodel2, trtest3, type = "class")
cm <- table(trtest3$Attrition, predict4)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#XGboosting - all variables with encoding all categorical variables into separate level variables into a sparse matrix (hyperparameter values based on manual trial and tests)
tr_data <- data.matrix(trtrain2[,-1])
tr_label <- trtrain2$Attrition
test_data <- data.matrix(trtest[,-1])
test_label <- trtest$Attrition
new_tr <- model.matrix(~.+0,data = trtrain2[,-1])
new_test <- model.matrix(~.+0,data = trtest[,-1])
new_labtr <- as.numeric(tr_label)-1
new_labte <- as.numeric(test_label)-1

dtrain <- xgb.DMatrix(data = new_tr, label = new_labtr)
set.seed(123)
xgb1 <- xgboost(data = dtrain, max.depth = 10, eta = 0.1, nthread = 2, nrounds = 1000, verbose = 1, objective = "binary:logistic")

dtest <- xgb.DMatrix(data = new_test, label = new_labte)
pred1<- predict(xgb1, dtest)
xgbpred1 <- ifelse (pred1 > 0.8,1,0)
cm <- table(new_labte, xgbpred)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#boost - subset (hyperparameter values based on manual trial and tests)
tr_data <- data.matrix(trtrain3[,-1])
tr_label <- trtrain3$Attrition
test_data <- data.matrix(trtest3[,-1])
test_label <- trtest3$Attrition
new_tr <- model.matrix(~.+0,data = trtrain3[,-1])
new_test <- model.matrix(~.+0,data = trtest3[,-1])
new_labtr <- as.numeric(tr_label)-1
new_labte <- as.numeric(test_label)-1

dtrain <- xgb.DMatrix(data = new_tr, label = new_labtr)
set.seed(123)
xgb1 <- xgboost(data = dtrain, max.depth = 2, eta = 0.1, nthread = 2, nrounds = 1000, verbose = 1, objective = "binary:logistic")

dtest <- xgb.DMatrix(data = new_test, label = new_labte)
pred2<- predict(xgb1, dtest)
xgbpred2 <- ifelse (pred1 > 0.8,1,0)
cm <- table(new_labte, xgbpred)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

##cross validation on xgb model for all variables

tr_data <- data.matrix(trtrain2[,-1])
tr_label <- trtrain2$Attrition
test_data <- data.matrix(trtest[,-1])
test_label <- trtest$Attrition
new_tr <- model.matrix(~.+0,data = trtrain2[,-1])
new_test <- model.matrix(~.+0,data = trtest[,-1])
new_labtr <- as.numeric(tr_label)-1
new_labte <- as.numeric(test_label)-1

dtrain <- xgb.DMatrix(data = new_tr, label = new_labtr)

xgb_grid_1 = expand.grid(nrounds = 1000,eta = c(0.2, 0.1, 0.01, 0.001), max_depth = c(2,3, 4, 6, 8, 10), gamma = 0, colsample_bytree = 1,
  min_child_weight = 1,subsample = 1)

xgb_trcontrol_1 = trainControl(method = "cv",number = 5,verboseIter = TRUE,returnResamp = "all", classProbs = TRUE,
summaryFunction = twoClassSummary,allowParallel = TRUE)

xgb_train_1 = train(
  x = as.matrix(new_tr),
  y = as.factor(tr_label),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

##plot to check ROC estimation for hyperparameter tuning
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

##prediction using cv model
pred6<- predict(xgb_train_1, new_test)
cm <- table(new_labte, pred6)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

##feature importance in final model
importance <- xgb.importance(feature_names = colnames(new_tr), model = xgb_train_1$finalModel)
xgb.ggplot.importance(importance, rel_to_first = TRUE, xlab = "Relative Importance")
