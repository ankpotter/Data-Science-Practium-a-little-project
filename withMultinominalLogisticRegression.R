library(plyr)
library(e1071)
library(ISLR)
library(leaps)
library(caret)
library(nnet)
library(MASS)
#load data, renaming columns
load('/Users/Pagliacci/Desktop/DSC450/samsungData.rda')
count(samsungData$activity)

colnames(samsungData)=1:length(colnames(samsungData))
colnames(samsungData)[length(colnames(samsungData))]='predicted'

#hold-out 7:3
nr=nrow(samsungData)
trainidx=sample(1:nr,0.7*nr)
train=samsungData[trainidx,]
test=samsungData[-trainidx,]

#mlr feature selection top 50 features based on varimp
model1=multinom(predicted~., data= train, MaxNWts=30000,trace=FALSE)
imp=varImp(model1)
imp$na=NA
imp=rownames(imp[order(-imp$Overall),])[1:50]
feat=gsub("\`", "", imp)



subsetTrainD=subset(train,select = c(feat, 'predicted'))
subsetTestD=subset(test,select = c(feat, 'predicted'))
model2=multinom(predicted~., data=subsetTrainD, MaxNWts=30000)
mean(predict(model2,newdata = subsetTestD)==subsetTestD$predicted)

#stepAIC takes forever carry with caution! aproximate 10mins
#this is a stepwise selection out of 50 features from above
step=stepAIC(model2,direction="both")
step$anova

#svm without selection
svm.fit=svm(as.factor(predicted)~. , kernel="radial", data=train, cost=10000, scale=FALSE, gamma=0.0001,probability=TRUE)
mean(predict(svm.fit, newdata=test)==test$predicted)
summary(svm.fit)

regfit.fwd=regsubsets(predicted~.,data=train,nvmax=19, method =" forward ")

class(svm.fit)
importance <- varImp(svm.fit)
