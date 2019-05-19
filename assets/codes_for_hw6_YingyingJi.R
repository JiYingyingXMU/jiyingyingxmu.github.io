## Code to accompany HW6 on 
## Microeconometrics
## Yingying Ji
## Stu.ID:15220162202134

install.packages("neuralnet")
install.packages("NeuralNetTools")
library(caret)
library(glmnet)
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(e1071)
library(nnet)
library(neuralnet)
library(NeuralNetTools)
library(gridExtra)
library(AER)
rm(list=ls())
data <- read.csv("C:/Users/15068/Desktop/Microeconometrics/HW6/weather.csv",header = T)
summary(data)
corrplot(cor(data))

# process
data[,1:16] = scale(data[,1:16]) # scale the data
data$RainTomorrow = as.factor(data$RainTomorrow)
nvar = ncol(data) - 1

# create training and test sets
set.seed(123)
train =  createDataPartition(data$RainTomorrow,p=0.5,list=F)
data_train = data[train,]
data_test = data[-train,]
ytrue = data_test$RainTomorrow

#######################
# Logistic Regression #
#######################
fit <- glm(RainTomorrow ~.,data_train,family='binomial')
summary(fit)
result=coeftest(fit)
sum(result[,4]>0.05) #4 is not sig.

# test err
phat = predict(fit,data_test,type="response")
yhat = as.numeric(phat > 0.5)
table(ytrue,yhat)
1-mean(yhat==ytrue) #misclassification error rate :0.147156

#######################
# Classification Tree #
#######################
set.seed(100)
fit = rpart(RainTomorrow ~.,data_train)
rpart.plot(fit,box.palette=list("Grays", "Reds"))

# test err
yhat = predict(fit,data_test,type="class") 
table(ytrue,yhat)
1-mean(yhat==ytrue) #misclassification error rate:0.1610315

#################
# Random Forest #
#################
set.seed(100)
fit=train(RainTomorrow~.,data=data_train,method="rf",
          trControl=trainControl(method = "cv"),
          tuneLength=10) #tuneLength: number of mtry to try
fit$bestTune
fit = randomForest(RainTomorrow ~.,data=data_train,mtry=8) #selected by CV

# test err
yhat = predict(fit,data_test) 
table(ytrue,yhat)
1-mean(yhat==ytrue) #misclassification error rate:0.1401322

varImpPlot(fit)

############
# Boosting #
############
ntree = 5000
data_boost = transform(data_train,RainTomorrow=as.numeric(RainTomorrow)-1)
fit = gbm(RainTomorrow~.,data_boost,distribution="adaboost",
          n.trees=ntree,
          interaction.depth = 10,
          shrinkage = 0.1) 
summary(fit)

# test error
phat = predict(fit,data_test,n.trees=ntree,type="response")
yhat = as.numeric(phat>0.5) 
table(ytrue,yhat)
1-mean(yhat==ytrue)  #0.143472,tree5000

#######
# SVM #
#######
set.seed (100)
fit= svm(RainTomorrow~.,data_train,kernel="radial",gamma=0.01,cost=30,scale=F) #selected by CV
yhat = predict(fit,data_test,type="class") 
table(ytrue,yhat)
1-mean(yhat==ytrue) #misclassification error rate:  0.1435753,0.01;

##############
# Neural Net #
##############
set.seed(100)
fit = nnet(RainTomorrow ~.,data=data_train,
           size=10,maxit=10000,MaxNWts=10000,decay=0.1) #selected by CV
yhat = predict(fit,data_test,type="class") 
table(ytrue,yhat)
1-mean(yhat==ytrue) #misclassification error rate:0.1422669

# visualize results
# vidualize network
plotnet(fit,alpha_val=.2,
        circle_col="hotpink",
        pos_col="burlywood",
        neg_col="darkgray")

# importance plots based on weights 
h = olden(fit)
h + coord_flip() + theme(axis.text=element_text(size=14),axis.title=element_text(size=14))

# partial dependence plots of selected vars (other vars fixed at median)
h1 = lekprofile(fit,xsel=c("Pressure9am"),group_vals=0.5) + 
  theme(legend.position="none",axis.title=element_blank())
h2 = lekprofile(fit,xsel=c("WindGustSpeed"),group_vals=0.5) + 
  theme(legend.position="none",axis.title=element_blank())
h3 = lekprofile(fit,xsel=c("Pressure3pm"),group_vals=0.5) + 
  theme(legend.position="none",axis.title=element_blank())
h4 = lekprofile(fit,xsel=c("Sunshine"),group_vals=0.5) + 
  theme(legend.position="none",axis.title=element_blank())
grid.arrange(h1,h2,h3,h4,ncol=2)
