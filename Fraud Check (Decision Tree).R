library(readr)
Fraud_check <- read_csv("Fraud_check.csv")
View(Fraud_check)
sum(is.na(Fraud_check))
boxplot(Fraud_check[,c(4,5)])
library(dplyr)
Fraud_check["Taxable.Income"] <- if_else(Fraud_check$Taxable.Income <= 30000,"Risky","Good")
Fraud_check["Taxable.Income"] <- factor(Fraud_check$Taxable.Income)

# spleeting data into training and testing
Fraud_check_Risky <- Fraud_check[Fraud_check$Taxable.Income == "Risky",] #124
Fraud_check_Good <- Fraud_check[Fraud_check$Taxable.Income == "Good",] #476

Fraud_check_train <- rbind(Fraud_check_Risky[1:99,],Fraud_check_Good[1:381,])
Fraud_check_test <- rbind(Fraud_check_Risky[100:124,],Fraud_check_Good[382:476,])


# Building a model using rpart on training data
library(rpart)
rpart_train <- rpart(Taxable.Income~.,data=Fraud_check_train) # decision tree model building
summary(rpart_train)

pred_train <- predict(rpart_train,Fraud_check_train)
pred_train <- data.frame(pred_train)
pred_train <- if_else(pred_train$Good > 0.5,"Good","Risky")
mean(pred_train==Fraud_check_train$Taxable.Income) # Training Accuracy = 79.38%

pred_train <- as.factor(pred_train)
Fraud_check_train["Taxable.Income"] <- as.factor(Fraud_check_train$Taxable.Income)
library(caret)
confusionMatrix(pred_train,Fraud_check_train$Taxable.Income) # Training Accuracy = 79.38%


pred_rpart_test <- predict(rpart_train,newdata=Fraud_check_test) # predicting on test data
pred_rpart_test <- data.frame(pred_rpart_test)
pred_rpart_test <- if_else(pred_rpart_test$Good > 0.5,"Good","Risky")
mean(pred_rpart_test==Fraud_check_test$Taxable.Income)  # Testing Accuracy = 79.17%

pred_rpart_test <- as.factor(pred_rpart_test)
Fraud_check_train["Taxable.Income"] <- as.factor(Fraud_check_test$Taxable.Income)
confusionMatrix(pred_rpart_test,Fraud_check_test$Taxable.Income)  # Testing Accuracy = 79.17%
