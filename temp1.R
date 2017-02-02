




#set seed to make this script repeatable
set.seed(1994)
# library(mlbench)
library(caret)
setwd('C:/Users/suzho/Desktop/practium')

load('./samsungData.rda')
colnames(samsungData) <- make.names(names(samsungData), unique = TRUE)
samsungData$activity <- as.factor(samsungData$activity)


##I think we should delete subject

# split=0.70
# trainIndex <- createDataPartition(samsungData$activity, p=split, list=FALSE)
# train <- samsungData[ trainIndex,]
# test <- samsungData[-trainIndex,]

## here are two different way to split training and testing data.
train <- subset(samsungData, samsungData$subject < 23)
test <- subset(samsungData, samsungData$subject >= 23)

train <- subset(samsungData, samsungData$subject > 5)
test <- subset(samsungData, samsungData$subject <= 5)

testActivity<-test[,563]
test<-test[,-563]



model4 <- train(activity~tBodyAcc.max...X  +fBodyAccJerk.energy...Z+tGravityAcc.mean...X+tGravityAcc.max...Y , data=train, method="rf",metric='Accuracy',ntree=10)
result<-predict(model4,test,type='raw')
truecase<-sum(result==testActivity)
accuracy<-truecase/dim(test)[1]
accuracy

model4 <- train(activity~tBodyAcc.max...X  +fBodyAccJerk.energy...Z+tGravityAcc.mean...X , data=train, method="rf",metric='Accuracy',ntree=10)
result<-predict(model4,test,type='raw')
truecase<-sum(result==testActivity)
accuracy<-truecase/dim(test)[1]
accuracy

 




# 
# model4 <- train(activity~tBodyAcc.max...X  +fBodyAccJerk.energy...Z +tGravityAcc.max...Y +tGravityAcc.mean...X , data=train, method="rf",metric='Accuracy',ntree=10)
# result<-predict(model4,test,type='raw')
# truecase<-sum(result==testActivity)
# accuracy<-truecase/dim(test)[1]
# accuracy
# 
# 
# 
# 6 classes: 'laying', 'sitting', 'standing', 'walk', 'walkdown', 'walkup' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 4694, 4694, 4694, 4694, 4694, 4694, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2   0.8922381  0.8702260
# 33   0.9684464  0.9620132
# 562   0.9578871  0.9492978
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 33. 
# > importance <- varImp(model3, scale=FALSE)
# > print(importance)
# rf variable importance
# # 
# # only 20 most important variables shown (out of 562)
# # 
# # Overall
# # tGravityAcc.max...X                196.98
# # tGravityAcc.mean...Y               180.93
# # tGravityAcc.max...Y                173.33
# # angle.Y.gravityMean.               144.47
# # fBodyAccMag.mad..                  127.75
# # tGravityAcc.min...Y                126.76
# # tGravityAcc.min...X                114.53
# # tGravityAcc.mean...X               114.07
# # fBodyAccJerk.mean...X              105.02
# # fBodyAcc.bandsEnergy...1.16        103.60
# # tBodyAcc.max...X                    87.66
# # fBodyAccJerk.bandsEnergy...1.16     83.63
# # tBodyAccJerk.energy...Y             82.93
# # tBodyGyroJerkMag.sma..              81.49
# # tBodyAccJerk.iqr...X                80.91
# # tGravityAcc.arCoeff...Y.3           80.32
# # fBodyAccJerk.bandsEnergy...1.24.1   79.02
# # fBodyAccJerk.mad...X                78.79
# # fBodyAccJerk.max...X                78.77
# # fBodyAcc.bandsEnergy...9.16         77.49
# 
# # train the model
# model3 <- train(activity~., data=train, method="rf",metric='Accuracy',ntree=10)
# 
# importance <- varImp(model3, scale=FALSE)
# # summarize importance
# print(importance)
# # 
# # tGravityAcc.max...X                196.98
# # tGravityAcc.mean...Y               180.93
# # tGravityAcc.max...Y                173.33
# 
# result<-predict(model3,test,type='raw')
# truecase<-sum(result==testActivity)
# accuracy<-truecase/dim(test)[1]
# accuracy
# 
# model4 <- train(activity~tBodyAcc.max...X +fBodyAccJerk.energy...Z +tGravityAcc.max...Y +tGravityAcc.mean...X , data=train, method="rf",metric='Accuracy',ntree=10)
# result<-predict(model4,test,type='raw')
# truecase<-sum(result==testActivity)
# accuracy<-truecase/dim(test)[1]
# accuracy
# 
# 
# +tGravityAcc.mean...X 
# tGravityAcc.max...X+
#   fBodyAccJerk.mean...X+
#   +
# fBodyAccJerk.max...X
# tGravityAcc.max...Y+angle.Y.gravityMean.+fBodyAccMag.mad..+
# 
# model4 <- train(activity~tGravityAcc.max...Y+tGravityAcc.max...X+tGravityAcc.max...Z, data=train, method="rf",metric='Accuracy',ntree=10)
# result<-predict(model4,test,type='raw')
# truecase<-sum(result==testActivity)
# accuracy<-truecase/dim(test)[1]
# accuracy
# 
# 
# model4 <- train(activity~tBodyAcc.max...X +tBodyAcc.max...X+tBodyAcc.max...X, data=train, method="rf",metric='Accuracy',ntree=10)
# 
# tGravityAcc.max...Y+tGravityAcc.max...X+tGravityAcc.max...Z
# 
# result<-predict(model4,test,type='raw')
# truecase<-sum(result==testActivity)
# accuracy<-truecase/dim(test)[1]
# accuracy
# 
# [1] "tBodyAcc.mean...X"                    "tBodyAcc.mean...Y"                    "tBodyAcc.mean...Z"                   
# [4] "tBodyAcc.std...X"                     "tBodyAcc.std...Y"                     "tBodyAcc.std...Z"                    
# [7] "tBodyAcc.mad...X"                     "tBodyAcc.mad...Y"                     "tBodyAcc.mad...Z"                    
# [10] "tBodyAcc.max...X"                     "tBodyAcc.max...Y"                     "tBodyAcc.max...Z"                    
# [13] "tBodyAcc.min...X"                     "tBodyAcc.min...Y"                     "tBodyAcc.min...Z"                    
# [16] "tBodyAcc.sma.."                       "tBodyAcc.energy...X"                  "tBodyAcc.energy...Y"                 
# [19] "tBodyAcc.energy...Z"                  "tBodyAcc.iqr...X"                     "tBodyAcc.iqr...Y"                    
# [22] "tBodyAcc.iqr...Z"                     "tBodyAcc.entropy...X"                 "tBodyAcc.entropy...Y"                
# [25] "tBodyAcc.entropy...Z"                 "tBodyAcc.arCoeff...X.1"               "tBodyAcc.arCoeff...X.2"              
# [28] "tBodyAcc.arCoeff...X.3"               "tBodyAcc.arCoeff...X.4"               "tBodyAcc.arCoeff...Y.1"              
# [31] "tBodyAcc.arCoeff...Y.2"               "tBodyAcc.arCoeff...Y.3"               "tBodyAcc.arCoeff...Y.4"              
# [34] "tBodyAcc.arCoeff...Z.1"               "tBodyAcc.arCoeff...Z.2"               "tBodyAcc.arCoeff...Z.3"              
# [37] "tBodyAcc.arCoeff...Z.4"               "tBodyAcc.correlation...X.Y"           "tBodyAcc.correlation...X.Z"          
# [40] "tBodyAcc.correlation...Y.Z"           "tGravityAcc.mean...X"                 "tGravityAcc.mean...Y"                
# [43] "tGravityAcc.mean...Z"                 "tGravityAcc.std...X"                  "tGravityAcc.std...Y"                 
# [46] "tGravityAcc.std...Z"                  "tGravityAcc.mad...X"                  "tGravityAcc.mad...Y"                 
# [49] "tGravityAcc.mad...Z"                  "tGravityAcc.max...X"                  "tGravityAcc.max...Y"                 
# [52] "tGravityAcc.max...Z"                  "tGravityAcc.min...X"                  "tGravityAcc.min...Y"                 
# [55] "tGravityAcc.min...Z"                  "tGravityAcc.sma.."                    "tGravityAcc.energy...X"              
# [58] "tGravityAcc.energy...Y"               "tGravityAcc.energy...Z"               "tGravityAcc.iqr...X"                 
# [61] "tGravityAcc.iqr...Y"                  "tGravityAcc.iqr...Z"                  "tGravityAcc.entropy...X"             
# [64] "tGravityAcc.entropy...Y"              "tGravityAcc.entropy...Z"              "tGravityAcc.arCoeff...X.1"           
# [67] "tGravityAcc.arCoeff...X.2"            "tGravityAcc.arCoeff...X.3"            "tGravityAcc.arCoeff...X.4"           
# [70] "tGravityAcc.arCoeff...Y.1"            "tGravityAcc.arCoeff...Y.2"            "tGravityAcc.arCoeff...Y.3"           
# [73] "tGravityAcc.arCoeff...Y.4"            "tGravityAcc.arCoeff...Z.1"            "tGravityAcc.arCoeff...Z.2"           
# [76] "tGravityAcc.arCoeff...Z.3"            "tGravityAcc.arCoeff...Z.4"            "tGravityAcc.correlation...X.Y"       
# [79] "tGravityAcc.correlation...X.Z"        "tGravityAcc.correlation...Y.Z"        "tBodyAccJerk.mean...X"               
# [82] "tBodyAccJerk.mean...Y"                "tBodyAccJerk.mean...Z"                "tBodyAccJerk.std...X"                
# [85] "tBodyAccJerk.std...Y"                 "tBodyAccJerk.std...Z"                 "tBodyAccJerk.mad...X"                
# [88] "tBodyAccJerk.mad...Y"                 "tBodyAccJerk.mad...Z"                 "tBodyAccJerk.max...X"                
# [91] "tBodyAccJerk.max...Y"                 "tBodyAccJerk.max...Z"                 "tBodyAccJerk.min...X"                
# [94] "tBodyAccJerk.min...Y"                 "tBodyAccJerk.min...Z"                 "tBodyAccJerk.sma.."                  
# [97] "tBodyAccJerk.energy...X"              "tBodyAccJerk.energy...Y"              "tBodyAccJerk.energy...Z"             
# [100] "tBodyAccJerk.iqr...X"                 "tBodyAccJerk.iqr...Y"                 "tBodyAccJerk.iqr...Z"                
# [103] "tBodyAccJerk.entropy...X"             "tBodyAccJerk.entropy...Y"             "tBodyAccJerk.entropy...Z"            
# [106] "tBodyAccJerk.arCoeff...X.1"           "tBodyAccJerk.arCoeff...X.2"           "tBodyAccJerk.arCoeff...X.3"          
# [109] "tBodyAccJerk.arCoeff...X.4"           "tBodyAccJerk.arCoeff...Y.1"           "tBodyAccJerk.arCoeff...Y.2"          
# [112] "tBodyAccJerk.arCoeff...Y.3"           "tBodyAccJerk.arCoeff...Y.4"           "tBodyAccJerk.arCoeff...Z.1"          
# [115] "tBodyAccJerk.arCoeff...Z.2"           "tBodyAccJerk.arCoeff...Z.3"           "tBodyAccJerk.arCoeff...Z.4"          
# [118] "tBodyAccJerk.correlation...X.Y"       "tBodyAccJerk.correlation...X.Z"       "tBodyAccJerk.correlation...Y.Z"      
# [121] "tBodyGyro.mean...X"                   "tBodyGyro.mean...Y"                   "tBodyGyro.mean...Z"                  
# [124] "tBodyGyro.std...X"                    "tBodyGyro.std...Y"                    "tBodyGyro.std...Z"                   
# [127] "tBodyGyro.mad...X"                    "tBodyGyro.mad...Y"                    "tBodyGyro.mad...Z"                   
# [130] "tBodyGyro.max...X"                    "tBodyGyro.max...Y"                    "tBodyGyro.max...Z"                   
# [133] "tBodyGyro.min...X"                    "tBodyGyro.min...Y"                    "tBodyGyro.min...Z"                   
# [136] "tBodyGyro.sma.."                      "tBodyGyro.energy...X"                 "tBodyGyro.energy...Y"                
# [139] "tBodyGyro.energy...Z"                 "tBodyGyro.iqr...X"                    "tBodyGyro.iqr...Y"                   
# [142] "tBodyGyro.iqr...Z"                    "tBodyGyro.entropy...X"                "tBodyGyro.entropy...Y"               
# [145] "tBodyGyro.entropy...Z"                "tBodyGyro.arCoeff...X.1"              "tBodyGyro.arCoeff...X.2"             
# [148] "tBodyGyro.arCoeff...X.3"              "tBodyGyro.arCoeff...X.4"              "tBodyGyro.arCoeff...Y.1"             
# [151] "tBodyGyro.arCoeff...Y.2"              "tBodyGyro.arCoeff...Y.3"              "tBodyGyro.arCoeff...Y.4"             
# [154] "tBodyGyro.arCoeff...Z.1"              "tBodyGyro.arCoeff...Z.2"              "tBodyGyro.arCoeff...Z.3"             
# [157] "tBodyGyro.arCoeff...Z.4"              "tBodyGyro.correlation...X.Y"          "tBodyGyro.correlation...X.Z"         
# [160] "tBodyGyro.correlation...Y.Z"          "tBodyGyroJerk.mean...X"               "tBodyGyroJerk.mean...Y"              
# [163] "tBodyGyroJerk.mean...Z"               "tBodyGyroJerk.std...X"                "tBodyGyroJerk.std...Y"               
# [166] "tBodyGyroJerk.std...Z"                "tBodyGyroJerk.mad...X"                "tBodyGyroJerk.mad...Y"               
# [169] "tBodyGyroJerk.mad...Z"                "tBodyGyroJerk.max...X"                "tBodyGyroJerk.max...Y"               
# [172] "tBodyGyroJerk.max...Z"                "tBodyGyroJerk.min...X"                "tBodyGyroJerk.min...Y"               
# [175] "tBodyGyroJerk.min...Z"                "tBodyGyroJerk.sma.."                  "tBodyGyroJerk.energy...X"            
# [178] "tBodyGyroJerk.energy...Y"             "tBodyGyroJerk.energy...Z"             "tBodyGyroJerk.iqr...X"               
# [181] "tBodyGyroJerk.iqr...Y"                "tBodyGyroJerk.iqr...Z"                "tBodyGyroJerk.entropy...X"           
# [184] "tBodyGyroJerk.entropy...Y"            "tBodyGyroJerk.entropy...Z"            "tBodyGyroJerk.arCoeff...X.1"         
# [187] "tBodyGyroJerk.arCoeff...X.2"          "tBodyGyroJerk.arCoeff...X.3"          "tBodyGyroJerk.arCoeff...X.4"         
# [190] "tBodyGyroJerk.arCoeff...Y.1"          "tBodyGyroJerk.arCoeff...Y.2"          "tBodyGyroJerk.arCoeff...Y.3"         
# [193] "tBodyGyroJerk.arCoeff...Y.4"          "tBodyGyroJerk.arCoeff...Z.1"          "tBodyGyroJerk.arCoeff...Z.2"         
# [196] "tBodyGyroJerk.arCoeff...Z.3"          "tBodyGyroJerk.arCoeff...Z.4"          "tBodyGyroJerk.correlation...X.Y"     
# [199] "tBodyGyroJerk.correlation...X.Z"      "tBodyGyroJerk.correlation...Y.Z"      "tBodyAccMag.mean.."                  
# [202] "tBodyAccMag.std.."                    "tBodyAccMag.mad.."                    "tBodyAccMag.max.."                   
# [205] "tBodyAccMag.min.."                    "tBodyAccMag.sma.."                    "tBodyAccMag.energy.."                
# [208] "tBodyAccMag.iqr.."                    "tBodyAccMag.entropy.."                "tBodyAccMag.arCoeff..1"              
# [211] "tBodyAccMag.arCoeff..2"               "tBodyAccMag.arCoeff..3"               "tBodyAccMag.arCoeff..4"              
# [214] "tGravityAccMag.mean.."                "tGravityAccMag.std.."                 "tGravityAccMag.mad.."                
# [217] "tGravityAccMag.max.."                 "tGravityAccMag.min.."                 "tGravityAccMag.sma.."                
# [220] "tGravityAccMag.energy.."              "tGravityAccMag.iqr.."                 "tGravityAccMag.entropy.."            
# [223] "tGravityAccMag.arCoeff..1"            "tGravityAccMag.arCoeff..2"            "tGravityAccMag.arCoeff..3"           
# [226] "tGravityAccMag.arCoeff..4"            "tBodyAccJerkMag.mean.."               "tBodyAccJerkMag.std.."               
# [229] "tBodyAccJerkMag.mad.."                "tBodyAccJerkMag.max.."                "tBodyAccJerkMag.min.."               
# [232] "tBodyAccJerkMag.sma.."                "tBodyAccJerkMag.energy.."             "tBodyAccJerkMag.iqr.."               
# [235] "tBodyAccJerkMag.entropy.."            "tBodyAccJerkMag.arCoeff..1"           "tBodyAccJerkMag.arCoeff..2"          
# [238] "tBodyAccJerkMag.arCoeff..3"           "tBodyAccJerkMag.arCoeff..4"           "tBodyGyroMag.mean.."                 
# [241] "tBodyGyroMag.std.."                   "tBodyGyroMag.mad.."                   "tBodyGyroMag.max.."                  
# [244] "tBodyGyroMag.min.."                   "tBodyGyroMag.sma.."                   "tBodyGyroMag.energy.."               
# [247] "tBodyGyroMag.iqr.."                   "tBodyGyroMag.entropy.."               "tBodyGyroMag.arCoeff..1"             
# [250] "tBodyGyroMag.arCoeff..2"              "tBodyGyroMag.arCoeff..3"              "tBodyGyroMag.arCoeff..4"             
# [253] "tBodyGyroJerkMag.mean.."              "tBodyGyroJerkMag.std.."               "tBodyGyroJerkMag.mad.."              
# [256] "tBodyGyroJerkMag.max.."               "tBodyGyroJerkMag.min.."               "tBodyGyroJerkMag.sma.."              
# [259] "tBodyGyroJerkMag.energy.."            "tBodyGyroJerkMag.iqr.."               "tBodyGyroJerkMag.entropy.."          
# [262] "tBodyGyroJerkMag.arCoeff..1"          "tBodyGyroJerkMag.arCoeff..2"          "tBodyGyroJerkMag.arCoeff..3"         
# [265] "tBodyGyroJerkMag.arCoeff..4"          "fBodyAcc.mean...X"                    "fBodyAcc.mean...Y"                   
# [268] "fBodyAcc.mean...Z"                    "fBodyAcc.std...X"                     "fBodyAcc.std...Y"                    
# [271] "fBodyAcc.std...Z"                     "fBodyAcc.mad...X"                     "fBodyAcc.mad...Y"                    
# [274] "fBodyAcc.mad...Z"                     "fBodyAcc.max...X"                     "fBodyAcc.max...Y"                    
# [277] "fBodyAcc.max...Z"                     "fBodyAcc.min...X"                     "fBodyAcc.min...Y"                    
# [280] "fBodyAcc.min...Z"                     "fBodyAcc.sma.."                       "fBodyAcc.energy...X"                 
# [283] "fBodyAcc.energy...Y"                  "fBodyAcc.energy...Z"                  "fBodyAcc.iqr...X"                    
# [286] "fBodyAcc.iqr...Y"                     "fBodyAcc.iqr...Z"                     "fBodyAcc.entropy...X"                
# [289] "fBodyAcc.entropy...Y"                 "fBodyAcc.entropy...Z"                 "fBodyAcc.maxInds.X"                  
# [292] "fBodyAcc.maxInds.Y"                   "fBodyAcc.maxInds.Z"                   "fBodyAcc.meanFreq...X"               
# [295] "fBodyAcc.meanFreq...Y"                "fBodyAcc.meanFreq...Z"                "fBodyAcc.skewness...X"               
# [298] "fBodyAcc.kurtosis...X"                "fBodyAcc.skewness...Y"                "fBodyAcc.kurtosis...Y"               
# [301] "fBodyAcc.skewness...Z"                "fBodyAcc.kurtosis...Z"                "fBodyAcc.bandsEnergy...1.8"          
# [304] "fBodyAcc.bandsEnergy...9.16"          "fBodyAcc.bandsEnergy...17.24"         "fBodyAcc.bandsEnergy...25.32"        
# [307] "fBodyAcc.bandsEnergy...33.40"         "fBodyAcc.bandsEnergy...41.48"         "fBodyAcc.bandsEnergy...49.56"        
# [310] "fBodyAcc.bandsEnergy...57.64"         "fBodyAcc.bandsEnergy...1.16"          "fBodyAcc.bandsEnergy...17.32"        
# [313] "fBodyAcc.bandsEnergy...33.48"         "fBodyAcc.bandsEnergy...49.64"         "fBodyAcc.bandsEnergy...1.24"         
# [316] "fBodyAcc.bandsEnergy...25.48"         "fBodyAcc.bandsEnergy...1.8.1"         "fBodyAcc.bandsEnergy...9.16.1"       
# [319] "fBodyAcc.bandsEnergy...17.24.1"       "fBodyAcc.bandsEnergy...25.32.1"       "fBodyAcc.bandsEnergy...33.40.1"      
# [322] "fBodyAcc.bandsEnergy...41.48.1"       "fBodyAcc.bandsEnergy...49.56.1"       "fBodyAcc.bandsEnergy...57.64.1"      
# [325] "fBodyAcc.bandsEnergy...1.16.1"        "fBodyAcc.bandsEnergy...17.32.1"       "fBodyAcc.bandsEnergy...33.48.1"      
# [328] "fBodyAcc.bandsEnergy...49.64.1"       "fBodyAcc.bandsEnergy...1.24.1"        "fBodyAcc.bandsEnergy...25.48.1"      
# [331] "fBodyAcc.bandsEnergy...1.8.2"         "fBodyAcc.bandsEnergy...9.16.2"        "fBodyAcc.bandsEnergy...17.24.2"      
# [334] "fBodyAcc.bandsEnergy...25.32.2"       "fBodyAcc.bandsEnergy...33.40.2"       "fBodyAcc.bandsEnergy...41.48.2"      
# [337] "fBodyAcc.bandsEnergy...49.56.2"       "fBodyAcc.bandsEnergy...57.64.2"       "fBodyAcc.bandsEnergy...1.16.2"       
# [340] "fBodyAcc.bandsEnergy...17.32.2"       "fBodyAcc.bandsEnergy...33.48.2"       "fBodyAcc.bandsEnergy...49.64.2"      
# [343] "fBodyAcc.bandsEnergy...1.24.2"        "fBodyAcc.bandsEnergy...25.48.2"       "fBodyAccJerk.mean...X"               
# [346] "fBodyAccJerk.mean...Y"                "fBodyAccJerk.mean...Z"                "fBodyAccJerk.std...X"                
# [349] "fBodyAccJerk.std...Y"                 "fBodyAccJerk.std...Z"                 "fBodyAccJerk.mad...X"                
# [352] "fBodyAccJerk.mad...Y"                 "fBodyAccJerk.mad...Z"                 "fBodyAccJerk.max...X"                
# [355] "fBodyAccJerk.max...Y"                 "fBodyAccJerk.max...Z"                 "fBodyAccJerk.min...X"                
# [358] "fBodyAccJerk.min...Y"                 "fBodyAccJerk.min...Z"                 "fBodyAccJerk.sma.."                  
# [361] "fBodyAccJerk.energy...X"              "fBodyAccJerk.energy...Y"              "fBodyAccJerk.energy...Z"             
# [364] "fBodyAccJerk.iqr...X"                 "fBodyAccJerk.iqr...Y"                 "fBodyAccJerk.iqr...Z"                
# [367] "fBodyAccJerk.entropy...X"             "fBodyAccJerk.entropy...Y"             "fBodyAccJerk.entropy...Z"            
# [370] "fBodyAccJerk.maxInds.X"               "fBodyAccJerk.maxInds.Y"               "fBodyAccJerk.maxInds.Z"              
# [373] "fBodyAccJerk.meanFreq...X"            "fBodyAccJerk.meanFreq...Y"            "fBodyAccJerk.meanFreq...Z"           
# [376] "fBodyAccJerk.skewness...X"            "fBodyAccJerk.kurtosis...X"            "fBodyAccJerk.skewness...Y"           
# [379] "fBodyAccJerk.kurtosis...Y"            "fBodyAccJerk.skewness...Z"            "fBodyAccJerk.kurtosis...Z"           
# [382] "fBodyAccJerk.bandsEnergy...1.8"       "fBodyAccJerk.bandsEnergy...9.16"      "fBodyAccJerk.bandsEnergy...17.24"    
# [385] "fBodyAccJerk.bandsEnergy...25.32"     "fBodyAccJerk.bandsEnergy...33.40"     "fBodyAccJerk.bandsEnergy...41.48"    
# [388] "fBodyAccJerk.bandsEnergy...49.56"     "fBodyAccJerk.bandsEnergy...57.64"     "fBodyAccJerk.bandsEnergy...1.16"     
# [391] "fBodyAccJerk.bandsEnergy...17.32"     "fBodyAccJerk.bandsEnergy...33.48"     "fBodyAccJerk.bandsEnergy...49.64"    
# [394] "fBodyAccJerk.bandsEnergy...1.24"      "fBodyAccJerk.bandsEnergy...25.48"     "fBodyAccJerk.bandsEnergy...1.8.1"    
# [397] "fBodyAccJerk.bandsEnergy...9.16.1"    "fBodyAccJerk.bandsEnergy...17.24.1"   "fBodyAccJerk.bandsEnergy...25.32.1"  
# [400] "fBodyAccJerk.bandsEnergy...33.40.1"   "fBodyAccJerk.bandsEnergy...41.48.1"   "fBodyAccJerk.bandsEnergy...49.56.1"  
# [403] "fBodyAccJerk.bandsEnergy...57.64.1"   "fBodyAccJerk.bandsEnergy...1.16.1"    "fBodyAccJerk.bandsEnergy...17.32.1"  
# [406] "fBodyAccJerk.bandsEnergy...33.48.1"   "fBodyAccJerk.bandsEnergy...49.64.1"   "fBodyAccJerk.bandsEnergy...1.24.1"   
# [409] "fBodyAccJerk.bandsEnergy...25.48.1"   "fBodyAccJerk.bandsEnergy...1.8.2"     "fBodyAccJerk.bandsEnergy...9.16.2"   
# [412] "fBodyAccJerk.bandsEnergy...17.24.2"   "fBodyAccJerk.bandsEnergy...25.32.2"   "fBodyAccJerk.bandsEnergy...33.40.2"  
# [415] "fBodyAccJerk.bandsEnergy...41.48.2"   "fBodyAccJerk.bandsEnergy...49.56.2"   "fBodyAccJerk.bandsEnergy...57.64.2"  
# [418] "fBodyAccJerk.bandsEnergy...1.16.2"    "fBodyAccJerk.bandsEnergy...17.32.2"   "fBodyAccJerk.bandsEnergy...33.48.2"  
# [421] "fBodyAccJerk.bandsEnergy...49.64.2"   "fBodyAccJerk.bandsEnergy...1.24.2"    "fBodyAccJerk.bandsEnergy...25.48.2"  
# [424] "fBodyGyro.mean...X"                   "fBodyGyro.mean...Y"                   "fBodyGyro.mean...Z"                  
# [427] "fBodyGyro.std...X"                    "fBodyGyro.std...Y"                    "fBodyGyro.std...Z"                   
# [430] "fBodyGyro.mad...X"                    "fBodyGyro.mad...Y"                    "fBodyGyro.mad...Z"                   
# [433] "fBodyGyro.max...X"                    "fBodyGyro.max...Y"                    "fBodyGyro.max...Z"                   
# [436] "fBodyGyro.min...X"                    "fBodyGyro.min...Y"                    "fBodyGyro.min...Z"                   
# [439] "fBodyGyro.sma.."                      "fBodyGyro.energy...X"                 "fBodyGyro.energy...Y"                
# [442] "fBodyGyro.energy...Z"                 "fBodyGyro.iqr...X"                    "fBodyGyro.iqr...Y"                   
# [445] "fBodyGyro.iqr...Z"                    "fBodyGyro.entropy...X"                "fBodyGyro.entropy...Y"               
# [448] "fBodyGyro.entropy...Z"                "fBodyGyro.maxInds.X"                  "fBodyGyro.maxInds.Y"                 
# [451] "fBodyGyro.maxInds.Z"                  "fBodyGyro.meanFreq...X"               "fBodyGyro.meanFreq...Y"              
# [454] "fBodyGyro.meanFreq...Z"               "fBodyGyro.skewness...X"               "fBodyGyro.kurtosis...X"              
# [457] "fBodyGyro.skewness...Y"               "fBodyGyro.kurtosis...Y"               "fBodyGyro.skewness...Z"              
# [460] "fBodyGyro.kurtosis...Z"               "fBodyGyro.bandsEnergy...1.8"          "fBodyGyro.bandsEnergy...9.16"        
# [463] "fBodyGyro.bandsEnergy...17.24"        "fBodyGyro.bandsEnergy...25.32"        "fBodyGyro.bandsEnergy...33.40"       
# [466] "fBodyGyro.bandsEnergy...41.48"        "fBodyGyro.bandsEnergy...49.56"        "fBodyGyro.bandsEnergy...57.64"       
# [469] "fBodyGyro.bandsEnergy...1.16"         "fBodyGyro.bandsEnergy...17.32"        "fBodyGyro.bandsEnergy...33.48"       
# [472] "fBodyGyro.bandsEnergy...49.64"        "fBodyGyro.bandsEnergy...1.24"         "fBodyGyro.bandsEnergy...25.48"       
# [475] "fBodyGyro.bandsEnergy...1.8.1"        "fBodyGyro.bandsEnergy...9.16.1"       "fBodyGyro.bandsEnergy...17.24.1"     
# [478] "fBodyGyro.bandsEnergy...25.32.1"      "fBodyGyro.bandsEnergy...33.40.1"      "fBodyGyro.bandsEnergy...41.48.1"     
# [481] "fBodyGyro.bandsEnergy...49.56.1"      "fBodyGyro.bandsEnergy...57.64.1"      "fBodyGyro.bandsEnergy...1.16.1"      
# [484] "fBodyGyro.bandsEnergy...17.32.1"      "fBodyGyro.bandsEnergy...33.48.1"      "fBodyGyro.bandsEnergy...49.64.1"     
# [487] "fBodyGyro.bandsEnergy...1.24.1"       "fBodyGyro.bandsEnergy...25.48.1"      "fBodyGyro.bandsEnergy...1.8.2"       
# [490] "fBodyGyro.bandsEnergy...9.16.2"       "fBodyGyro.bandsEnergy...17.24.2"      "fBodyGyro.bandsEnergy...25.32.2"     
# [493] "fBodyGyro.bandsEnergy...33.40.2"      "fBodyGyro.bandsEnergy...41.48.2"      "fBodyGyro.bandsEnergy...49.56.2"     
# [496] "fBodyGyro.bandsEnergy...57.64.2"      "fBodyGyro.bandsEnergy...1.16.2"       "fBodyGyro.bandsEnergy...17.32.2"     
# [499] "fBodyGyro.bandsEnergy...33.48.2"      "fBodyGyro.bandsEnergy...49.64.2"      "fBodyGyro.bandsEnergy...1.24.2"      
# [502] "fBodyGyro.bandsEnergy...25.48.2"      "fBodyAccMag.mean.."                   "fBodyAccMag.std.."                   
# [505] "fBodyAccMag.mad.."                    "fBodyAccMag.max.."                    "fBodyAccMag.min.."                   
# [508] "fBodyAccMag.sma.."                    "fBodyAccMag.energy.."                 "fBodyAccMag.iqr.."                   
# [511] "fBodyAccMag.entropy.."                "fBodyAccMag.maxInds"                  "fBodyAccMag.meanFreq.."              
# [514] "fBodyAccMag.skewness.."               "fBodyAccMag.kurtosis.."               "fBodyBodyAccJerkMag.mean.."          
# [517] "fBodyBodyAccJerkMag.std.."            "fBodyBodyAccJerkMag.mad.."            "fBodyBodyAccJerkMag.max.."           
# [520] "fBodyBodyAccJerkMag.min.."            "fBodyBodyAccJerkMag.sma.."            "fBodyBodyAccJerkMag.energy.."        
# [523] "fBodyBodyAccJerkMag.iqr.."            "fBodyBodyAccJerkMag.entropy.."        "fBodyBodyAccJerkMag.maxInds"         
# [526] "fBodyBodyAccJerkMag.meanFreq.."       "fBodyBodyAccJerkMag.skewness.."       "fBodyBodyAccJerkMag.kurtosis.."      
# [529] "fBodyBodyGyroMag.mean.."              "fBodyBodyGyroMag.std.."               "fBodyBodyGyroMag.mad.."              
# [532] "fBodyBodyGyroMag.max.."               "fBodyBodyGyroMag.min.."               "fBodyBodyGyroMag.sma.."              
# [535] "fBodyBodyGyroMag.energy.."            "fBodyBodyGyroMag.iqr.."               "fBodyBodyGyroMag.entropy.."          
# [538] "fBodyBodyGyroMag.maxInds"             "fBodyBodyGyroMag.meanFreq.."          "fBodyBodyGyroMag.skewness.."         
# [541] "fBodyBodyGyroMag.kurtosis.."          "fBodyBodyGyroJerkMag.mean.."          "fBodyBodyGyroJerkMag.std.."          
# [544] "fBodyBodyGyroJerkMag.mad.."           "fBodyBodyGyroJerkMag.max.."           "fBodyBodyGyroJerkMag.min.."          
# [547] "fBodyBodyGyroJerkMag.sma.."           "fBodyBodyGyroJerkMag.energy.."        "fBodyBodyGyroJerkMag.iqr.."          
# [550] "fBodyBodyGyroJerkMag.entropy.."       "fBodyBodyGyroJerkMag.maxInds"         "fBodyBodyGyroJerkMag.meanFreq.."     
# [553] "fBodyBodyGyroJerkMag.skewness.."      "fBodyBodyGyroJerkMag.kurtosis.."      "angle.tBodyAccMean.gravity."         
# [556] "angle.tBodyAccJerkMean..gravityMean." "angle.tBodyGyroMean.gravityMean."     "angle.tBodyGyroJerkMean.gravityMean."
# [559] "angle.X.gravityMean."                 "angle.Y.gravityMean."                 "angle.Z.gravityMean."                
# [562] "subject"                              "activity"                            
# 
# 
# 
# model5 <- train(activity~tGravityAcc.max...X+tGravityAcc.mean...Y, data=train, method="rf",metric='Accuracy',ntree=10)
# result<-predict(model5,test,type='raw')
# truecase<-sum(result==testActivity)
# accuracy<-truecase/dim(test)[1]
# accuracy
# 
# 
# 
# 
# 
# 
# 
# 
# library(rpart)
# model1<-rpart(activity~.,data = train,method='class')
# 
# summary(model1)
# plot(model1)
# text(model1)
# printcp(model1)
# plotcp(model1)
# 
# model2<-prune(model1,cp=0.02)
# summary(model2)
# printcp(model2)