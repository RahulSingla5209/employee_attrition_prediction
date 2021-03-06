rm(list = ls())

library(infotheo)
library(dplyr)
library(ggplot2)
library(corrplot)
library(glmnet)

input_data = read.csv('full_data.csv', header = T)

## remove dummy variables

# empolyee count, Ober18, Standard hours
input_data = input_data[, -c(9, 22, 27)]

num_obs = dim(input_data)[1]

X = input_data[,-2]
Y = input_data[,2]

# fix dataframe types
categorical_var_indices = c(2, 4, 6, 7, 9, 10, 12, 13, 14, 
                            15, 16, 20, 22, 23, 24, 27)

for (i in categorical_var_indices){
  X[,i] = as.factor(X[,i])
  X[is.na(X[,i]),i] <- mode(X[,i])
}
for (i in seq(dim(X)[2])){
  if(!i %in% categorical_var_indices)
  {
    X[,i] = as.numeric(X[,i])
    X[is.na(X[,i]),i] <- median(X[,i], na.rm = T)
  }
}

Y = as.factor(Y)

### EDA
input_data %>% 
  ggplot(aes(x = Attrition, y = MonthlyIncome, color = Attrition)) + 
  geom_boxplot() + ylab("Monthly  Income($)")
input_data %>% 
  ggplot(aes(x = Attrition, y = YearsAtCompany, color = Attrition)) + 
  geom_boxplot() + ylab("Years at Current Company")
input_data %>% 
  ggplot(aes(x = Attrition, y = YearsWithCurrManager, color = Attrition)) + 
  geom_boxplot() + ylab("Years with Current Manager")

mi = data.frame(var = character(), score = double())

for (i in 1:dim(X)[2])
{
  if(i == 3) next
  new_data = data.frame(var = names(X)[i], score = mutinformation(X[,i], Y))
  mi = rbind(mi, new_data)
}

### Categorical data correlation

n = length(categorical_var_indices)

high_corr = data.frame(var1 = character(),
                  var2 = character(),
                  p_value = double())
for (i in seq(n))
{
  for (j in seq(i+1, n))
  {
    if(j > n) next
    if(i > n) next
    
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
p = dim(X)[2]
for (i in seq(p))
{
  for (j in seq(i+1,p))
  {
    if(j > p)
    {
      next
    }
    if ((i %in% categorical_var_indices) | (j %in% categorical_var_indices))
    {
      next
    }
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

processed_data = cbind(X,Y)

#train_size = 0.75
train_indices = c(1:1029)

wt = rep(1, length(train_indices))
wt[Y[train_indices] == "Yes"] = 3

decision_boundary = 0.3

######### logistic fitting - baseline ################
glm.fit = glm(Y ~ ., data = processed_data, family = "binomial", 
              subset = train_indices)
glm.probs <- predict(glm.fit,type = "response", 
                     newdata = processed_data[-train_indices,])
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + .2, y=-0.05, 
           label="Decision Boundary", color="black")

######## logistic fitting - with different class weights ###############


glm.fit1 = glm(Y ~ ., data = processed_data[train_indices,], 
              family = "binomial", weights = wt)
glm.probs <- predict(glm.fit1,type = "response", 
                     newdata = processed_data[-train_indices,])
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + 0.2, y=-0.05, 
           label="Decision Boundary", color="black")


######## logistic fitting - lasso ###############

alpha = 1
lambda = 0.01
lasso_model_matric = model.matrix(~ .,data = X)

glm.fit2 = glmnet(lasso_model_matric, Y, subset = train_indices, 
                  family = "binomial", alpha = alpha)
glm.probs <- predict(glm.fit2,type = "response", 
                     newx = lasso_model_matric[-train_indices,], 
                     s = lambda)
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + 0.2, y=-0.05, 
           label="Decision Boundary", color="black")
coef(glm.fit2, s = lambda)




######## lr mode with interaction ##############

###### backward without weights###########
lr_models = model.matrix(Y ~ ., data = processed_data)
lr_models = data.frame(Y, lr_models)

lr_models_train = lr_models[train_indices,]
lr_models_test = lr_models[-train_indices,]

full1 = glm(Y ~ ., data = lr_models_train, family = "binomial")
null1 = glm(Y ~ 1, data = lr_models_train, family = "binomial")

regBackward = step(full1, direction="backward", k = log(num_obs))
glm.probs <- predict(regBackward,type = "response", 
                     newdata = lr_models_test)
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + 0.2, y=-0.05, 
           label="Decision Boundary", color="black")

mat  = cor(X[,-categorical_var_indices])
corrplot(mat, method="circle")

#############backward with validation set - without interaction##########
lr_models2 = model.matrix(Y ~ ., data = processed_data)
lr_models2 = data.frame(Y, lr_models2)

lr_models_train1 = lr_models2[train_indices,]
lr_models_test1 = lr_models2[-train_indices,]

full2 = glm(Y ~ ., data = lr_models_train1, 
            family = "binomial", weights = wt)
null2 = glm(Y ~ 1, data = lr_models_train1, 
            family = "binomial", weights = wt)
regBackward1 = step(full2, direction="backward", k = log(num_obs))
glm.probs <- predict(regBackward1,type = "response", 
                     newdata = lr_models_test1)
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + 0.2, y=-0.05, 
           label="Decision Boundary", color="black")