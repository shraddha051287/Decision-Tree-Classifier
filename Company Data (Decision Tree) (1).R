library(readr)
Company_Data <- read_csv("Company_Data.csv")
View(Company_Data)
summary(Company_Data) # all quaritles, mean, minimum and maximum of all variables in the company data  
boxplot(Company_Data[,-c(7,10:11)]) #boxplot of all numeric variables in the company data
#some variables contains outliers 
library(stats)
quantile(Company_Data$Sales,0.33) #6.03 
quantile(Company_Data$Sales,0.66) #8.6568
?quantile
library(dplyr)
Company_Data["Sales"] <- if_else(Company_Data$Sales<=6.03,"low",if_else(Company_Data$Sales<=8.6568,"medium","high"))  
#converting sales variable into chategorical variable with three levels
str(Company_Data)
# Building a regression tree using rpart 


# spleeting data into training and testing
sales_low <- Company_Data[Company_Data$Sales=="low",] #133
sales_medium <- Company_Data[Company_Data$Sales=="medium",] #131
sales_high <- Company_Data[Company_Data$Sales=="high",] #136
Company_data_train <- rbind(sales_low[1:100,],sales_medium[1:100,],sales_high[1:100,])
Company_data_test <- rbind(sales_low[101:133,],sales_medium[101:131,],sales_high[101:136,])


# Building a model using rpart on training data
library(rpart)
rpart_train <- rpart(Sales~.,data=Company_data_train) # decision tree model building
plot(rpart_train)
text(rpart_train)
summary(rpart_train)

pred_train <- predict(rpart_train,Company_data_train)
pred_train <- data.frame(pred_train)
pred_train <- if_else(pred_train$low > 0.33333,"low",if_else(pred_train$medium > 0.33333,"medium","high"))
mean(Company_data_train$Sales==pred_train) # Training Accuracy = 74%

pred_train <- as.factor(pred_train)
Company_data_train["Sales"] <- as.factor(Company_data_train$Sales)
library(caret)
confusionMatrix(pred_train,Company_data_train$Sales) # Training Accuracy = 74%

pred_rpart_test <- predict(rpart_train,newdata=Company_data_test) # predicting on test data
pred_rpart_test <- data.frame(pred_rpart_test)
pred_rpart_test <- if_else(pred_rpart_test$low > 0.33333,"low",if_else(pred_rpart_test$medium > 0.33333,"medium","high"))
mean(pred_rpart_test==Company_data_test$Sales)  #Testing Accuracy = 54% 

pred_rpart_test <- as.factor(pred_rpart_test)
Company_data_test["Sales"] <- as.factor(Company_data_test$Sales)
confusionMatrix(pred_rpart_test,Company_data_test$Sales)  #Testing Accuracy = 54% 
