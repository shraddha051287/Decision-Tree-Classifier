data(iris)  #import iris data
str(iris) 
sum(is.na(iris))
# there are no missing values in the iris data
summary(iris)
boxplot(iris[,-5])   #boxplot of all the variables in the iris data
# sepal width(one variable in the iris data) contains some outliers  
# sepal length and sepal width are slightly left tailed(negatively skewed) and right tailed(positively skewed) respectively.
# petal length and petal width are left tailed(negatively skewed). 
box <- boxplot(iris$Sepal.Width)$out    #outliers
# some outliers are present in the variable sepal width 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50

iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

library(party) # ctree function is in party package for ploting decision tree
irisct_train <- ctree(Species ~ .,data = iris_train)  #writing decision tree model for ploting decision tree 
irisct_train
plot(irisct_train)  #here is the decision tree plot using ctree function.
# Training accuracy
pred_train <- predict(irisct_train,iris_train)

mean(iris_train$Species==pred_train) # Training Accuracy = 97.33% 

library(caret)
confusionMatrix(pred_train,iris_train$Species) # Training Accuracy = 97.33%

predct_test <- predict(irisct_train,newdata=iris_test) # predicting on test data
mean(predct_test==iris_test$Species) # Testing Accuracy = 94.66%  

confusionMatrix(predct_test,iris_test$Species) # Testing Accuracy = 94.66%  

