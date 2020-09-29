# Libraries required 
library(readr)
library(caTools) # splitting data into train and test
library(class)   # for using knn function
library(caret)   # for confusion matrix

#loading the data
zoo <- read.csv(file.choose())
summary(zoo)
zoo$animal.name <- as.numeric(zoo$animal.name)
names(zoo$type)

# table of types of animals in zoo
table(zoo$type) # there are 7 types of animals

#proportion of the entries in the type of animals
round(prop.table(table(zoo$type))*100,1)

# as the data is of binomial or factor type no need of normalization
#creating trainin and testing data
set.seed(100)
split <- sample.split(zoo,SplitRatio = 0.8) # 80% for train 20% for test

train <- subset(zoo,split=='TRUE')
test <- subset(zoo,split=='FALSE')

# Building knn model on train data set

pred <- knn(train = train,test = train, train$type, k=9)
tab <- table(pred,train$type)
acc <- (sum(diag(tab))/length(train$type))*100

install.packages('gmodels')
library(gmodels)

CrossTable(x=train$type,y=pred,prop.chisq = FALSE)
