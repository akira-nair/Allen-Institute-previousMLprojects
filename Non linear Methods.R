DataBook<-read.csv("RestrictedDataBook.csv")

EmrgDataBook<-read.csv("EgData.csv")

attach(DataBook)
attach(EmrgDataBook)

attach(RestrictedDataBook)
attach(EmergencyData)

library(ggplot2)
library(reshape2)
library(ggpubr)
library(grid)
library(gridExtra)
library(rpart)
library(mgcv)
library(caret)
library(klaR)
library(xgboost)
library(randomForest)
library(MLmetrics)
#assigns data to a numeric list
WeightData <- as.numeric(as.character(WeightORCA))
PreGData <- `Last.Preoperative.Glucose.Value`
DMData <- `DM.type`
HeightData <- as.numeric(as.character(HeightORCA))
ASAData <- ASAVAL
AgeData <- Age
PeakGData <- `Peak.glucose.value`
hbData <- (`HbA1C..Last.one.within.6.mo.`)
hbData <- as.numeric(as.character(hbData))
PriGData <- `X1st.Intraop.glucose.value`
IData <- `Preop.Insulin`
OHAData <- `Preop.OralHyperglycemic.Agent`
ClassData <- Pat1entClass
#ProblemsData <- PreOperativeProblemList
#ProblemsData <- toupper(ProblemsData)
EMRGData <- EMRG
SurgeryData <- AppointmentTypeText
AnesData <- as.numeric(AnesCPT)
SexData <- `Gender`
Duration <- AppointmentDuration
#Restricting outliers from data
WeightData[WeightData > 500 | WeightData == 0] <- NA
PreGData[PreGData >500] <- NA
PeakGData[PeakGData > 500] <- NA
PriGData[PriGData > 500] <- NA
DMData[is.na(DMData) ] <- 0
#calculates BMI from weight and height Data
BMIData = (WeightData)/((HeightData*0.01)^2)
BMIData[BMIData > 60 | BMIData < 5] <- NA
ASAData[ASAData ==6] <- NA
hbData[is.na(hbData) & DMData == 0 ] <- 5.0
hbData[is.na(hbData) & DMData == 1 ] <- 6.5
hbData[is.na(hbData) & DMData == 2 ] <- 6.5
PreGData[is.na(PreGData)] <- 110
SurgeryData <- na.omit(SurgeryData)


#####DUMMY
options(na.action = 'na.pass')

sex.dummy<-model.matrix(~factor(SexData))
sexm.dummy<-sex.dummy[,2]

asa.dummy<-model.matrix(~factor(ASAData))
asa2.dummy<- asa.dummy[,2]
asa3.dummy<- asa.dummy[,3]
asa4.dummy<- asa.dummy[,4]
asa5.dummy<- asa.dummy[,5]

i.dummy<-model.matrix(~factor(IData))
iyes.dummy<-i.dummy[,2]

dt.dummy<-model.matrix(~factor(dtcategories))
dt1.dummy<-dt.dummy[,2]
dt2.dummy<-dt.dummy[,3]

oha.dummy<-model.matrix(~factor(OHAData))
ohayes.dummy <- oha.dummy[,2]

class.dummy<-model.matrix(~factor(ClassData))
classin.dummy <- class.dummy[,2]

emrg.dummy<-model.matrix(~factor(emrgcategories))
emrgyes.dummy <- emrg.dummy[,2]

surgery.dummy<-model.matrix(~factor(SurgeryData))
cardio.dummy <- surgery.dummy[,2]
general.dummy <- surgery.dummy[,3]
gynecology.dummy <- surgery.dummy[,4]
neurology.dummy <- surgery.dummy[,5]
oral.dummy <- surgery.dummy[,6]
orthopedic.dummy <- surgery.dummy[,7]
otolaryncology.dummy <- surgery.dummy[,8]
plastic.dummy <- surgery.dummy[,9]
thoracic.dummy <- surgery.dummy[,10]
transplant.dummy <- surgery.dummy[,11]
urology.dummy <- surgery.dummy[,12]
vascular.dummy <- surgery.dummy[,13]

anes.dummy<-model.matrix(~factor(anescategories))
intracranial.dummy <- anes.dummy[,2]
cardiac.dummy <- anes.dummy[,3]
liverx.dummy <- anes.dummy[,4]
spinal.dummy <- anes.dummy[,5]


### Regression
alldata = data.frame(PeakGData, AgeData, BMIData, PreGData, hbData, Duration, 
                     sexm.dummy, asa2.dummy, asa3.dummy, asa4.dummy, asa5.dummy, iyes.dummy, 
                     dt1.dummy, dt2.dummy, ohayes.dummy, classin.dummy, emrgyes.dummy, 
                     cardio.dummy, general.dummy, gynecology.dummy, neurology.dummy,
                     oral.dummy, orthopedic.dummy, otolaryncology.dummy, plastic.dummy,
                     thoracic.dummy, transplant.dummy, urology.dummy, vascular.dummy,
                     intracranial.dummy, cardiac.dummy, liverx.dummy, spinal.dummy)
alldata <- alldata[complete.cases(alldata),]
split = 0.8
dataIndex <- createDataPartition(alldata$PeakGData, p=split, list=FALSE)
data_train <- alldata[dataIndex,]
data_test <- alldata[-dataIndex,]

# Random Forest
# PeakG.rf <- randomForest(PeakGData ~ . , data=data_train)
# predictionsrf <- predict(PeakG.rf, data_test)
# print(1-MAPE(predictionsrf, data_test$PeakGData))

# XG Boost

PeakG.xg = xgboost(data = data.matrix(data_train[-1]), label = data_train$PeakGData, nrounds = 50, max_depth = 3, eta = 0.3, gamma = 10)
predictionsxg <- predict(PeakG.xg, data.matrix(data_test[-1]))
PeakG.rf = randomForest(PeakGData ~ ., data=data_train, max_features = 10)
predictionsrf <- predict(PeakG.rf, data_test[-1])
folds = createFolds(data_train$PeakGData, k = 10)
cv1 = lapply(folds, function(x) { #cross validation with 10 folds
  train_fold = data_train[-x,]
  test_fold = data_train[x,]
  predictions <- predict(PeakG.xg, data.matrix(test_fold[-1]))
  accuracy <- 1-MAPE(test_fold$PeakGData,predictions)
  return(accuracy)

})
cat("Validation Accuracy: ", mean(as.numeric(cv1)), "\n")

cat("Test Accuracy: ", (1-MAPE(data_test$PeakGData, predictionsxg)), "\n")


#poisson
#PeakG.ps = glm(formula = PeakGData ~ data_train[-1],data = data_train, family = poisson)
PeakG.rp = rpart(formula = PeakGData ~ data_train[-1], data = data_train)

predictionsrp <- predict(PeakG.rp, data_test[-1])

cat("Test Accuracy rp: ", (100-MAPE(data_test$PeakGData, predictionsrp)), "\n")



alldataPassP <- subset(data_train, select = -c(otolaryncology.dummy, transplant.dummy))
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
# print("VIF execution: ")
# vif_func(in_frame = alldataPassP, thresh = 4, trace = T)


## Stepwise Regression
alldataPassVIF <- subset(alldataPassP, select = -c(asa3.dummy, neurology.dummy, cardiac.dummy))
# print(colnames(alldataPassVIF))
# print("Stepwise Regression: ")
# fullmodel <- (lm(PeakGData ~ ., data = alldataPassVIF))
# stepwisemodel <- stepAIC(fullmodel, direction = "both", trace = FALSE)
#print(lm.beta(stepwisemodel))



## Final Linear model
alldataPassStep <- subset(alldataPassVIF, select = -c(AgeData, BMIData, iyes.dummy, ohayes.dummy, general.dummy, gynecology.dummy, oral.dummy, orthopedic.dummy, urology.dummy, PreGData, hbData))
LinearModel <- lm(PeakGData ~ ., data = alldataPassStep)



predictionsln <- predict(LinearModel, data_test[-1])
View(data.frame(predictionsln, data_test[,1]))

errorln <- data.frame(predictionsln, data_test[,1])
errorln$error <- errorln$predictionsln - errorln$data_test
errorln$error[errorln$error >= 100] <- NA
errorln$error[errorln$error <= -100] <- NA
errorlnhist <- hist(errorln$error, breaks = 20, main = "Error of Sample Test with Linear", xlab = "error", freq= TRUE, ylim = c(0,200), xlim = c(-100,100))


errorxg <- data.frame(predictionsxg, data_test[,1])
errorxg$error <- errorxg$predictionsxg - errorxg$data_test
errorxg$error[errorxg$error >= 100] <- NA
errorxg$error[errorxg$error <= -100] <- NA
errorxghist <- hist(errorxg$error, breaks = 20, main = "Error of Sample Test with Xg Boost", xlab = "error", freq= TRUE, ylim = c(0,200), xlim = c(-100,100))

errorrf <- data.frame(predictionsrf, data_test[,1])
errorrf$error <- errorrf$predictionsrf - errorrf$data_test
errorrf$error[errorrf$error >= 100] <- NA
errorrf$error[errorrf$error <= -100] <- NA
errorrfhist <- hist(errorrf$error, breaks = 20, main = "Error of Sample Test with Random Forest", xlab = "error", freq= TRUE, ylim = c(0,200), xlim = c(-100,100))

finalerrordf <- data.frame(data_test[,1], predictionsln, predictionsxg, predictionsrf)
quantile(errorln$error, c(0.25,0.50,0.75), na.rm=TRUE)
quantile(errorxg$error, c(0.25,0.50,0.75), na.rm=TRUE)
quantile(errorrf$error, c(0.25,0.50,0.75), na.rm=TRUE)
# PeakG.rf = randomForest(PeakGData ~ ., data=data_train, max_features = 10)
# predictionsrf <- predict(PeakG.rf, data_test[-1])
# 
# cv2 = lapply(folds, function(x) { #cross validation with 10 folds
#   train_fold = data_train[-x,]
#   test_fold = data_train[x,]
#   predictions <- predict(PeakG.rf, test_fold[-1])
#   accuracy <- 1-MAPE(test_fold$PeakGData,predictions)
#   return(accuracy)
# 
# })
# cat("Validation Accuracy: ", mean(as.numeric(cv2)), "\n")
# 
# cat("Test Accuracy: ", (1-MAPE(data_test$PeakGData, predictionsrf)), "\n")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ### Classification
# # 
# #classifying hyperglycemia
# HyperGData <- PeakGData
# HyperGData[HyperGData <= 140] <- 0
# HyperGData[HyperGData > 140] <- 1
# HyperGData <- as.factor(HyperGData)
# 
# alldatana2 = data.frame(HyperGData, AgeData, BMIData, hbData, PreGData, Duration,
#                        sexm.dummy, asa2.dummy, asa3.dummy, asa4.dummy, asa5.dummy, iyes.dummy,
#                        dt1.dummy, dt2.dummy, ohayes.dummy, classin.dummy, emrgyes.dummy,
#                        cardio.dummy, general.dummy, gynecology.dummy, neurology.dummy,
#                        oral.dummy, orthopedic.dummy, otolaryncology.dummy, plastic.dummy,
#                        thoracic.dummy, transplant.dummy, urology.dummy, vascular.dummy,
#                        intracranial.dummy, cardiac.dummy, liverx.dummy, spinal.dummy)
# 
# 
# alldata2 <- alldatana2[complete.cases(alldatana2),]
# dataIndex <- createDataPartition(alldata2$HyperGData, p=split, list=FALSE)
# data_train <- alldata2[dataIndex,]
# data_test <- alldata2[-dataIndex,]
# 
# 
# # XG Boost
# HyperG.xg = xgboost(data = as.matrix(alldata2[-1]), label = alldata2$HyperGData, nrounds = 10)
# predictionsxg <- predict(HyperG.xg, as.matrix(data_test[-1]), outputmargin = FALSE)
# predictionsxg <- as.numeric(predictionsxg >= 0.5)
# confusionMatrix(table(data_test[,1], predictionsxg))
# # varImp(HyperG.xg)
# # folds = createFolds(alldata2$HyperGData, k = 10)
# # cv1 = lapply(folds, function(x) { #cross validation with 10 folds
# #   train_fold = alldata2[-x,]
# #   test_fold = alldata2[x,]
# #   predictions <- predict(HyperG.xg, as.matrix(test_fold[-1]))
# #   predictions <- (predictions >= 1.5)
# #   conMat = table(test_fold[, 1], predictions)
# #   accuracy = (conMat[1,1] + conMat[2,2])/(conMat[1,1] + conMat[2,2] + conMat[1,2] + conMat[2,1])
# #   return(accuracy)
# #   return(conMat)
# # })
# # cat("Accuracy of XG Boost: ", mean(as.numeric(cv1)), "\n")
# 
# 
# # Random Forest
# alldata2$HyperGData = as.factor(alldata$HyperGData)
# alldata2$HyperGData = as.factor(alldata$HyperGData)
# HyperG.rf = randomForest(HyperGData ~ ., data=data_train, ntry = 50)
# # varImp(HyperG.rf)
# # cv2 = lapply(folds, function(x) {
# #   train_fold = alldata2[-x,]
# #   test_fold = alldata2[x,]
# #   predictions <- predict(HyperG.rf, test_fold[-1])
# #   conMat = table(test_fold[, 1], predictions)
# #   accuracy = (conMat[1,1] + conMat[2,2])/(conMat[1,1] + conMat[2,2] + conMat[1,2] + conMat[2,1])
# #   return(accuracy)
# #   return(conMat)
# # })
# # cat("Accuracy of Random Forest: ", mean(as.numeric(cv2)), "\n")
# predictionsrf <- predict(HyperG.rf, data_test[-1])
# confusionMatrix(table(predictionsrf, data_test[,1]))
