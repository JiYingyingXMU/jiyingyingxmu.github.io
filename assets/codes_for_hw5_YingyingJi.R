library(caret)
library(glmnet)
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)
library(gbm)
library(AER)
rm(list=ls())
data <- read.csv("C:/Users/15068/Desktop/Microeconometrics/HW5/Employee.csv",header = T)

# See the attrition rate

data$Attrition = as.factor(data$Attrition)
dim(data)
sum(data$Attrition==1)/nrow(data) # Attrition rate in the data
set.seed(123)
train =  createDataPartition(data$Attrition,p=0.7,list=F)
data_train = data[train,]
data_test = data[-train,]
sum(data_train$Attrition==1)/nrow(data_train) # Attrition rate in the training data
sum(data_test$Attrition==1)/nrow(data_test) # Attrition rate in the test data


# Logistic Regression
fit = glm(Attrition~.,data_train,family="binomial")
summary(fit)
result=coeftest(fit)
result[1:4,]
sum(result[,4]>0.05)

# Test error in logistic regression
ytrue = data_test$Attrition
phat = predict(fit,data_test,type="response")
yhat = as.numeric(phat > 0.5)
table(ytrue,yhat)
1-mean(yhat==ytrue) #misclassification error rate


# Claasification Tree
set.seed(100)
fit0 = rpart(Attrition~.,data_train,control=rpart.control(cp=0))
fit = prune(fit0,cp=fit0$cptable[which.min(fit0$cptable[,"xerror"]),"CP"])
rpart.plot(fit,box.palette=list("Grays","Reds"))

# Test error in classification tree
yhat = predict(fit,data_test,type="class")
table(ytrue,yhat)
1-mean(yhat==ytrue)#misclassification error rate

# Random Forest 

set.seed(100)
fit=train(Attrition~.,data=data_train,method="rf",
          trControl=trainControl(method = "cv"),
          tuneLength=10)
fit$bestTune

fit = randomForest(Attrition~.,data=data_train,mtry=19)

# Test error in random forest
yhat = predict(fit,data_test)
table(ytrue,yhat)
1-mean(yhat==ytrue) 

# See the importance of variables
varImpPlot(fit)

# Boosting
set.seed(100)
data_boost = transform(data_train,Attrition=as.numeric(Attrition)-1) 
ntree=5000
fit = gbm(Attrition~.,data_boost, distribution="adaboost",
          n.trees=ntree,interaction.depth=10,shrinkage=0.1)
summary(fit)

# Test error in boosting
phat=predict(fit,data_test,n.trees=5000,type = "response")
yhat=as.numeric(phat>0.5)
table(ytrue,yhat)
1-mean(yhat==ytrue)

