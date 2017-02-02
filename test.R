#set seed to make this script repeatable
set.seed(1995)
# library(mlbench)
library(caret)


load('/Users/Pagliacci/Desktop/DSC450/samsungData.rda')
colnames(samsungData) <- make.names(names(samsungData), unique = TRUE)
samsungData$activity <- as.factor(samsungData$activity)


##I think we should delete subject
split=0.70
trainIndex <- createDataPartition(samsungData$activity, p=split, list=FALSE)
train <- samsungData[ trainIndex,]
test <- samsungData[-trainIndex,]

testActivity<-test[,563]
test<-test[,-563]




# train the model
model3 <- train(activity~., data=train, method="rf",metric='Accuracy',ntree=10)

importance <- varImp(model3, scale=FALSE)
# summarize importance
print(importance)


result<-predict(model3,test,type='raw')
truecase<-sum(result==testActivity)
accuracy<-truecase/dim(test)[1]
accuracy

model4 <- train(activity~tGravityAcc.min...X +angle.Y.gravityMean.+angle.X.gravityMean. , data=train, method="rf",metric='Accuracy',ntree=10)
result<-predict(model4,test,type='raw')
truecase<-sum(result==testActivity)
accuracy<-truecase/dim(test)[1]
accuracy

model5 <- train(activity~tGravityAcc.min...X +angle.Y.gravityMean., data=train, method="rf",metric='Accuracy',ntree=10)
result<-predict(model5,test,type='raw')
truecase<-sum(result==testActivity)
accuracy<-truecase/dim(test)[1]
accuracy








library(rpart)
model1<-rpart(activity~.,data = train,method='class')

summary(model1)
plot(model1)
text(model1)
printcp(model1)
plotcp(model1)

model2<-prune(model1,cp=0.02)
summary(model2)
printcp(model2)




library(tree)
dec1 <- tree(activity ~ ., data = train)
plot(dec1)
text(dec1)
title("Decision Tree")
summary(dec1)

result<-predict(dec1,test,type='class')
truecase<-sum(result==testActivity)
accuracy<-truecase/dim(test)[1]
accuracy

xtrain<-train[,-563]
xlabel<-train[,563]
method = 'vglmAdjCat'
model <- train(x=xtrain,y=xlabel, method="gbm",n.trees=20,metric='Kappa')



control <- trainControl(method="none")






control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid,)
print(rf_default)




# estimate variable importance   , trControl=control

# plot importance
plot(importance)


# unique(samsungData$subject)
# subject_number<-unique(samsungData$subject)
# #divide the data into 70% training data and 30% testing data
# length(subject_number)*.3
# train <- subset(samsungData, samsungData$subject < 23)
# test <- subset(samsungData, samsungData$subject >= 23)






# importance <- varImp(dec1, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)





# tGravityAcc.min...X          tGravityAcc.mean...X          angle.X.gravityMean. 
# 5                             5                             5 
# tGravityAcc.energy...X           tGravityAcc.max...X      tBodyAccJerk.entropy...X 
# 5                             5                             5 
# tBodyAccJerk.iqr...X          tBodyAccJerk.mad...X            tBodyAccJerk.sma.. 
# 5                             5                             5 
# tBodyAccJerkMag.mean..         tBodyAccJerkMag.sma..        tGravityAcc.energy...Y 
# 5                             5 



