#required libraries
library(readr)   # for loading the data set
library(caTools) # for splitting the data into train and test
library(class)   # for using knn function
library(caret)   # for using confusion matrix
#install.packages('corrplot')
library(corrplot) # for ploting correlation plot

#load the data set 
glass <- read.csv(file.choose())
summary(glass)

#normalizing the data
glass1 <- scale(glass[,-10])
anyNA(glass)
glass <- cbind(glass1,glass[10])

corrplot(cor(glass),method = 'number') # it explains the correlation of the variables

# spliting data into train and test

set.seed(100)
sample <- sample.split(glass$Type,SplitRatio = 0.70)
train <- subset(glass,sample=='TRUE')
test <- subset(glass,sample=='FALSE')

# KNN model
model <- knn(train[1:9],test[1:9],train$Type,k=1)
error <- mean(model!=test$Type)
confusionMatrix(model,as.factor(test$Type))
################################################################
#Confusion Matrix and Statistics

           #Reference
#Prediction  1  2  3  5  6  7
         #1 17  3  3  0  0  1
         #2  1 17  0  1  0  1
         #3  3  2  2  0  0  0
         #5  0  1  0  3  0  0
         #6  0  0  0  0  3  1
         #7  0  0  0  0  0  6



#Overall Statistics

#Accuracy : 0.7385          
#95% CI : (0.6146, 0.8397)
#No Information Rate : 0.3538          
#P-Value [Acc > NIR] : 3.019e-10       

#Kappa : 0.6485          

#Mcnemar's Test P-Value : NA 

#########################################################################

# KNN model
model1 <- knn(train[1:9],test[1:9],train$Type,k=3)
error <- mean(model1!=test$Type)
confusionMatrix(model1,as.factor(test$Type))
 # accuracy is 75%

