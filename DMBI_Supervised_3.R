# clear environment
rm(list = ls())
# set your working directory to source file location
# read in the data 
hd_data = read.csv("heart_disease_data_2.csv")

# clean it (replace missing vals with N/A)
library(dplyr)
#install.packages("naniar") 
library(naniar)
library(tidyverse)
hd_data = hd_data %>% replace_with_na(replace = list(ca = c(-9, "?",9), thal = c(-9, "?"), 
slope = c(-9, "?"), exang = c(-9, "?"), thalach = c(-9, "?"), restecg = c(-9, "?"), 
fbs = c(-9, "?"), chol = c(-9), trestbps = c(-9, "?"), oldpeak = c("?")))

# take a 70% sample size for training
smp_size = floor(0.7 * nrow(hd_data))

# seperate into test and train data
set.seed(243)
train <- sample(seq_len(nrow(hd_data)), size = smp_size)                        
hd_train <- hd_data[train, ] #training data
hd_test = hd_data[-train, ] #testing data

# turn the dummy/factor variables into factor variables
attach(hd_train)
fbs = as.factor(fbs)
exang = as.factor(exang)
slope = as.factor(slope)
ca = as.factor(ca)
class = as.factor(class)


#LOGISTIC REGRESSION
# run logistic regression on train data using the 14 explanatory vars. 
attach(hd_train)
logit1 <- glm(class~Age + Sex + Cp+chol+fbs+slope+ca+exang+thal
              +restecg, family = binomial(link = "logit"), 
              maxit = 100, hd_train)
plot(logit1)
summary(logit1)
plot( hd_train$Age, hd_train$class)
# goodness-of-fit for logit model
fitted = fitted.values(logit1)
fitted = round(fitted, digits = 2)
fitted
predicted = predict(logit1, newdata = hd_test, type = "response", na.action = na.omit)
predicted = round(predicted, digits = 2)
predicted = as.numeric(predicted >= 0.5)
predicted
sum(predicted)
length(predicted)
hd_test_for_accuracy = na.omit(hd_test)
predicted_correct = sum(predicted == hd_test_for_accuracy$class)
predicted_correct
predicted == hd_test_for_accuracy$class #just to check
accuracy_logit = predicted_correct/nrow(hd_test_for_accuracy)
accuracy_logit
#rm(fbs)

#confusion matrix for train data
install.packages("caret")
library(caret)
hd_train_without_na = na.omit(hd_train)
pred_value1 <- predict(logit1, newdata = hd_train_without_na, type = "response")
pred_value1
pred_value_train <- round(pred_value1, digits = 2)
pred_value_train

# use caret and compute a confusion matrix
actual_value_train <- hd_train_without_na$class
actual_value_train
confusion_matrix_train = table(actual_value_train, pred_value_train)
confusion_matrix_train

#misclassification rate for train data
misclassification_error_rate_train = 1 - sum(diag(confusion_matrix))/sum(confusion_matrix)
misclassification_error_rate_train

#confusion matrix for test data
library(caret)
pred_value_test <- predict(logit1, newdata = hd_test_for_accuracy, type = "response")
#pred_value <- round(pred_value, digits = 2)
# use caret and compute a confusion matrix
actual_value_test <- hd_test_for_accuracy$class
confusion_matrix_test = table(actual_value_test, pred_value_test)
confusion_matrix_test

#misclassification rate for test data
misclassification_error_rate_test = 1 - sum(diag(confusion_matrix))/sum(confusion_matrix)
misclassification_error_rate_test

#checking for multicollinearity using VIF
library(car)
vif(logit1)



# DECISION TREE ANALYSIS
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
tree_fit <- rpart(class~Age + Sex + Cp+chol+fbs+slope+ca+exang+thal
                  +restecg, data = hd_train, method = 'class')
#tree_model <- rpart(formula = class~Age + Sex + Cp + trestbps+chol+fbs+restecg
                   #+thalach+exang+oldpeak+slope+ca
                   #+thal + num,data = hd_train, method = "class")
rpart.plot(tree_fit, extra = 106)
summary(tree_fit)
printcp(tree_fit)
plotcp(tree_fit)

#prediction for train data
predict_data_tree <- predict(tree_fit, hd_train, type = 'class')
predict_data_tree
predicted_correct_tree = sum(predict_data_tree == hd_train$class)
predicted_correct_tree
nrow(hd_train)
accuracy_tree = predicted_correct_tree/nrow(hd_train)
accuracy_tree # tree accuracy = ~87%, logit accuracy ~80.6%
table_mat <- table(hd_train$class, predict_data_tree)
table_mat

#Accuracy Test for train data
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#prediction for test data
predict_data_test_tree <- predict(tree_fit, hd_test, type = 'class')
table_mat1 <- table(hd_test$class, predict_data_test_tree)
table_mat1
predicted_correct_tree_test = sum(predict_data_test_tree == hd_test$class)
predicted_correct_tree_test
nrow(hd_test)

#Accuracy Test for test data
accuracy_Test_tree <- sum(diag(table_mat1)) / sum(table_mat1)
accuracy_Test_tree

#confusion matrix for train data
library(caret)
pred_value_tree_train <- predict(tree_model, newdata = hd_train, type = "response")
#pred_value <- round(pred_value, digits = 2)
# use caret and compute a confusion matrix
actual_value_tree_train <- hd_train$class
confusion_matrix_tree_train = table(actual_value_tree_train, pred_value_tree_train)
confusion_matrix_tree_train

#misclassification rate for train data
misclassification_error_rate_tree_train = 1 - sum(diag(confusion_matrix_tree_train))/sum(confusion_matrix_tree_train)
misclassification_error_rate_tree_train

#confusion matrix for test data
library(caret)
pred_value_tree_test <- predict(tree_model, newdata = hd_test, type = "response")
#pred_value <- round(pred_value, digits = 2)
# use caret and compute a confusion matrix
actual_value_tree_test <- hd_test$class
confusion_matrix_tree_test = table(actual_value_tree_test, pred_value_tree_test)
confusion_matrix_tree_test

#misclassification rate for test data
misclassification_error_rate_tree_test = 1 - sum(diag(confusion_matrix_tree_test))/sum(confusion_matrix_tree_test)
misclassification_error_rate_tree_test



# RANDOM FOREST ANALYSIS
install.packages("randomForest")
library(randomForest)
set.seed(101)
#heart.rf <- randomForest(class~Age + Sex + Cp+chol+fbs+slope+ca+exang+thal
                       #  +restecg , data = hd_train_without_na)
#summary(heart.rf)
#plot(heart.rf)
oob.err <- double(10)
test.err <- double(10)

for (mtry in 1:10)
  {
  rf <- randomForest(class~Age + Sex + Cp+chol+fbs+slope+ca+exang+thal+restecg
    ,data =hd_train_without_na, mtry = mtry, ntree = 212)
  
  oob.err[mtry] = rf$mse[212] #Error of all Trees fitted
  
  #Predictions on Test Set for each Tree
  pred <- predict(rf, hd_train_without_na)
  
  #Mean Squared Test Error
  test.err[mtry] = with(hd_train_without_na, mean( (class - pred)^2)) 
  
  #printing the output to the console
  print(oob.err[mtry])
  print(test.err[mtry])
  
}
oob.err
test.err
matplot(1:mtry , cbind(oob.err,test.err), pch = 19 , col = c("red","blue"),
        type = "b",ylab = "Mean Squared Error",
        xlab = "Number of Predictors Considered at each Split")
legend("topright",legend = c("Out of Bag Error"), ("Test Error"),
       pch = 19, col = c("red", "blue"), cex= 0.8)

#confusion matrix for train data
library(caret)
pred_value_forest_train <- predict(rf, newdata = hd_train_without_na, type = "response")
pred_value_forest_train
predicted_correct_forest_train = sum(pred_value_forest_train == hd_train_without_na$class)
predicted_correct_forest_train #correctly predicted 
#pred_value <- round(pred_value, digits = 2)
#use caret and compute a confusion matrix
actual_value_forest_train<- hd_train_without_na$class
confusion_matrix_forest_train = table(actual_value_forest_train, pred_value_forest_train)
confusion_matrix_forest_train

#misclassification rate for train data
misclassification_error_rate_forest_train = 1 - sum(diag(confusion_matrix_forest_train))/sum(confusion_matrix_forest_train)
misclassification_error_rate_forest_train

#confusion matrix for test data
library(caret)
pred_value1 <- predict(rf, newdata = hd_test_for_accuracy, type = "response")
pred_value1
#pred_value <- round(pred_value, digits = 2)
# use caret and compute a confusion matrix
actual_value <- hd_test_for_accuracy$class
confusion_matrix = table(actual_value, pred_value1)
confusion_matrix

#misclassification rate for test data
misclassification_error_rate = 1 - sum(diag(confusion_matrix))/sum(confusion_matrix)
misclassification_error_rate
