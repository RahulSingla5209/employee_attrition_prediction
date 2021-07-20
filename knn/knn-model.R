library(class) ## a library with lots of classification tools
library(kknn) ## knn library

attrition_full_data <- read.csv('full-attrition-data.csv')
attrition_full_data[attrition_full_data == ''] <- NA

n <- dim(attrition_full_data)[1]

ind <- sample(1:n,1000)

Y <- attrition_full_data$Attrition[ind]
attrition_data <- attrition_full_data[ind,]

n <- dim(attrition_data)[1]

kcv <- 10
n0 <- round(n/kcv,0)

out_acc <- matrix(0,kcv,100)
out_error_rate <- matrix(0,kcv,100)

used <- NULL
set <- 1:n

Y <- as.numeric(Y)

train <- data.frame(Y, as.numeric(attrition_data$BusinessTravel), 
                    as.numeric(attrition_data$EnvironmentSatisfaction),
                   as.numeric(attrition_data$JobSatisfaction), 
                   as.numeric(attrition_data$MaritalStatus),
                   as.numeric(attrition_data$NumCompaniesWorked), 
                   as.numeric(attrition_data$OverTime))
test <- data.frame(Y, as.numeric(attrition_data$BusinessTravel), 
                   as.numeric(attrition_data$EnvironmentSatisfaction),
                   as.numeric(attrition_data$JobSatisfaction), 
                   as.numeric(attrition_data$MaritalStatus),
                   as.numeric(attrition_data$NumCompaniesWorked), 
                   as.numeric(attrition_data$OverTime))

ind <- order(test[,1])
test <- test[ind,]

train <- na.omit(train)
test <- na.omit(test)

train_labels <- train$Y
test_labels <- test$Y

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i <- train[-val,]
  test_i <- test[val,]
  train_i_labels <- train[1][-val,]
  test_i_labels <- test[1][val, ]
  
  for(i in 1:100){
    
    near <- knn(train=train_i,test=test_i,cl=train_i_labels, k=i)
    tab <- table(near,test_i_labels)
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
    error_rate <- mean(test_i_labels!=near)
    
    out_acc[j,i] <- accuracy(tab)
    out_error_rate[j,i] <- error_rate
  }
  
  used <- union(used,val)
  set <- (1:n)[-used]
  
  cat(j,'\n')
}

mean_acc <- apply(out_acc,2,mean)
par(mfrow=c(1,1))
plot(1:100,mean_acc,type="l",ylab="Mean Accuracy",col=4,lwd=2,main="Employee Attrition (knn)",xlab="K")


mean_error_rate <- apply(out_error_rate,2,mean)
par(mfrow=c(1,1))
plot(1:100,mean_error_rate,type="l",ylab="Mean Error Rate",col=4,lwd=2,main="Employee Attrition (knn)",xlab="K")
