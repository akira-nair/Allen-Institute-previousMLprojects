# Final Hyperglycemia App Model

# performance analytics

## APP
library(caret)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(stringr)
library(xgboost)
library(randomForest)
library(xlsx)
library(MLmetrics)

# IMPORT DATA
DataBook<-read.csv("RestrictedDataBook.csv")

EmrgDataBook<-read.csv("EgData.csv")

#A_COHORT<-data.frame(read.csv("cohort_a.csv"))
A_COHORT<- read_excel("REVISEDDATAXL.xls", sheet = 1)
B_COHORT<-data.frame(read.csv("cohort_b.csv"))

attach(DataBook)
attach(EmrgDataBook)
# DATA SETUP
WeightData <- as.numeric(WeightORCA)
PreGData <- `Last.Preoperative.Glucose.Value`
DMData <- `DM.type`
HeightData <- as.numeric(HeightORCA)
ASAData <- ASAVAL
AgeData <- Age
PeakGData <- `Peak.glucose.value`
hbData <- `HbA1C..Last.one.within.6.mo.`
print(hbData)
PriGData <- `X1st.Intraop.glucose.value`
IData <- `Preop.Insulin`
OHAData <- `Preop.OralHyperglycemic.Agent`
ClassData <- Pat1entClass
#ProblemsData <- PreOperativeProblemList
#ProblemsData <- toupper(ProblemsData)
EMRGData <- EMRG
SurgeryData <- AppointmentTypeText
AnesData <- as.numeric(AnesCPT)
AnesDatacopy<-AnesData
AnesData[210 <= AnesData & AnesData <= 222] <- 1 #intracranial
AnesData[560 <= AnesData & AnesData <= 580] <- 2 #CPB
AnesData[AnesData==796] <- 3 #livertx
AnesData[600 <= AnesData & AnesData <= 670] <- 4 #spinal
AnesData[AnesData!= 1 & AnesData!= 2 & AnesData!= 3 & AnesData!= 4 | is.na(AnesData)] <- 0
SexData <- `Gender`
Duration <- AppointmentDuration

## Data Cleanup
WeightData[WeightData > 500 | WeightData < 20] <- NA # Weight must be between 20 and 500 lbs
PreGData[PreGData >500] <- NA # Glucose must be less than 500
PeakGData[PeakGData > 500] <- NA
PriGData[PriGData > 500] <- NA
DMData[is.na(DMData) ] <- 0 # Missing values are converted to 0; patient is not diabetic
SurgeryData <- str_replace(SurgeryData, "PLASTICS", "PLASTIC")
ASAData[ASAData ==6] <- NA # Ignore ASA 6 values
SexData[SexData =="U"] <- NA
#hbData[is.na(hbData) & DMData == 0 ] <- 5.0
#hbData[is.na(hbData) & DMData == 1 ] <- 6.5
#hbData[is.na(hbData) & DMData == 2 ] <- 6.5
PreGData[is.na(PreGData)] <- 110

#calculates BMI from weight and height Data
BMIData = (WeightData)/((HeightData*0.01)^2)
BMIData[BMIData > 60 | BMIData < 5] <- NA


#Categories 
dtcategories <- cut(DMData, 3, labels = c("UD", "1", "2"))
emrgcategories <- cut(EMRGData, 2, labels = c("No EMRG", "EMRG"))
anescategories <- cut(AnesData, 5, labels = c("Other", "Intracranial", "CPB", "Liver TX", "Spinal"))



# DUMMY Creation
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








# # # M A I N C O D E # # #
#hbData <- as.numeric(hbData)
options(stringsAsFactors = FALSE)
hbData <- as.character(hbData)
hbDatacopy <- hbData
hbData[hbData == ''] <- NA
hbDatacopy[hbDatacopy == '']<-'0'
hbData[hbData == '<4.0'] <- '4.0'
hbDatacopy[hbDatacopy == '<4.0'] <- '4.0'
#HyperGData <- as.factor(HyperGData)
alldatana = data.frame(PeakGData, AgeData, BMIData, PreGData, hbData, Duration, 
                       sexm.dummy, asa2.dummy, asa3.dummy, asa4.dummy, asa5.dummy, iyes.dummy, 
                       dt1.dummy, dt2.dummy, ohayes.dummy, classin.dummy, emrgyes.dummy, 
                       cardio.dummy, general.dummy, gynecology.dummy, neurology.dummy,
                       oral.dummy, orthopedic.dummy, otolaryncology.dummy, plastic.dummy,
                       thoracic.dummy, transplant.dummy, urology.dummy, vascular.dummy,
                       intracranial.dummy, cardiac.dummy, liverx.dummy, spinal.dummy)
alldatana$hbData[is.na(alldatana$hbData) & ((alldatana$dt1.dummy == 0) & (alldatana$dt2.dummy ==0))] <- 5.0
alldatana$hbData[is.na(alldatana$hbData) & (alldatana$dt1.dummy == 1) ] <- 6.5
alldatana$hbData[is.na(alldatana$hbData) & (alldatana$dt2.dummy == 1)] <- 6.5
alldatana$hbData = as.numeric(alldatana$hbData)
#alldatana$hbData<-as.double(alldatana$hbData)
alldata <- alldatana[complete.cases(alldatana),]

#alldata <- data.matrix(alldata)
split=0.8
dataIndex <- createDataPartition(alldata$PeakGData, p=split, list=FALSE)
data_train <- alldata[dataIndex,]
data_test <- alldata[-dataIndex,]
extraalldata <- data.frame(alldatana, hbDatacopy)
extraalldata <- extraalldata[complete.cases(extraalldata),]
extraalldata <- extraalldata[-dataIndex,]

# change first item in data.matrix to 'all data' for actual model
appPeakG.xg = xgboost(data = data.matrix(data_train[-1]), label = data_train$PeakGData, nrounds = 50, max_depth = 7, eta = 0.1, gamma = 10)
#appPeakG.xg = randomForest(PeakGData ~ ., data=data_train)


# app.PeakG.rf <- randomForest(PeakGData ~ AgeData + BMIData + PreGData + hbData + Duration +
#                 sexm.dummy + asa2.dummy + asa3.dummy + asa4.dummy + asa5.dummy + iyes.dummy +
#                 dt1.dummy + dt2.dummy + ohayes.dummy + classin.dummy + emrgyes.dummy+
#                 cardio.dummy + general.dummy + gynecology.dummy + neurology.dummy +
#                 oral.dummy + orthopedic.dummy + otolaryncology.dummy + plastic.dummy +
#                 thoracic.dummy + transplant.dummy + urology.dummy + vascular.dummy +
#                 intracranial.dummy + cardiac.dummy + liverx.dummy + spinal.dummy, data = alldata)


predictions <- predict(appPeakG.xg, as.matrix(data_test[-1]))
predictiondf <- data.frame(predictions,data_test, extraalldata$hbDatacopy)
A_COHORT <- predictiondf[sample(nrow(predictiondf), 100),]
A_COHORT$iyes.dummy[A_COHORT$iyes.dummy == 1] <- 'YES'
A_COHORT$iyes.dummy[A_COHORT$iyes.dummy == 0] <- 'NO'
A_COHORT$ohayes.dummy[A_COHORT$ohayes.dummy == 1] <- 'YES'
A_COHORT$ohayes.dummy[A_COHORT$ohayes.dummy == 0] <- 'NO'
A_COHORT$classin.dummy[A_COHORT$classin.dummy == 1] <- 'IN'
A_COHORT$classin.dummy[A_COHORT$classin.dummy == 0] <- 'OUT'
A_COHORT$emrgyes.dummy [A_COHORT$emrgyes.dummy == 1] <- 'YES'
A_COHORT$emrgyes.dummy[A_COHORT$emrgyes.dummy == 0] <- 'NO'
cohort_b <- predictiondf[sample(nrow(predictiondf), 50),]
#for (i in colnames(data_test)){
 # print(i)
  #print(wilcox.test(cohort_a[[i]],cohort_b[[i]], correct=FALSE))
#}

A_COHORT$asaCOMP<-0
A_COHORT$sexCOMP<-'F'
A_COHORT$diabCOMP <- 'NA'
A_COHORT$surgeryCOMP <- 'ANES OFF SITE'
A_COHORT$anesCOMP <- 'OTHER'

A_COHORT$asaCOMP <- with(A_COHORT, ifelse(A_COHORT$asa2.dummy == 1, 2, ifelse(A_COHORT$asa3.dummy == 1, 3, ifelse(A_COHORT$asa4.dummy == 1, 4, ifelse(A_COHORT$asa5.dummy == 1, 5, 1)) )))

A_COHORT$sexCOMP <- with(A_COHORT, ifelse(A_COHORT$sexm.dummy == 1, 'M', 'F'))

A_COHORT$diabCOMP <- with(A_COHORT, ifelse(A_COHORT$dt1.dummy == 1, '1', ifelse(A_COHORT$dt2.dummy == 1, '2', 'NA')))

A_COHORT$surgeryCOMP <- with(A_COHORT, ifelse(A_COHORT$general.dummy == 1, 'GENERAL', ifelse(A_COHORT$transplant.dummy == 1, 'TRANSPLANT', ifelse(A_COHORT$thoracic.dummy == 1,'THORACIC', ifelse(A_COHORT$otolaryncology.dummy == 1, 'OTOLARYNGOLOGY', ifelse(A_COHORT$neurology.dummy == 1, 'NEUROSURGERY', ifelse(A_COHORT$plastic.dummy == 1, 'PLASTIC', ifelse(A_COHORT$cardio.dummy == 1, 'CARDIO SURGERY', ifelse(A_COHORT$urology.dummy == 1 , 'UROLOGY', ifelse(A_COHORT$vascular.dummy == 1, 'VASCULAR SURGERY', ifelse(A_COHORT$orthopedic.dummy == 1, 'ORTHOPEDIC SURGERY', ifelse(A_COHORT$gynecology.dummy == 1, 'GYNECOLOGY', ifelse(A_COHORT$oral.dummy == 1, 'ORAL SURGERY', 'ANES OFF SITE'))))))))))) ))

A_COHORT$anesCOMP <- with(A_COHORT, ifelse(A_COHORT$intracranial.dummy == 1, 'INTRACRANIAL', ifelse(A_COHORT$cardiac.dummy == 1, 'CARDIAC', ifelse(A_COHORT$liverx.dummy == 1, 'LIVERX', ifelse(A_COHORT$spinal.dummy == 1, ' SPINAL', 'OTHER')))))





print(1- MAPE(predictiondf[,1], predictiondf[,2]))



A_COHORT$asaCOMP<-0
A_COHORT$sexCOMP<-'F'
A_COHORT$diabCOMP <- 'NA'
A_COHORT$surgeryCOMP <- 'ANES OFF SITE'
A_COHORT$anesCOMP <- 'OTHER'

A_COHORT$asaCOMP <- with(A_COHORT, ifelse(A_COHORT$asa2.dummy == 1, 2, ifelse(A_COHORT$asa3.dummy == 1, 3, ifelse(A_COHORT$asa4.dummy == 1, 4, ifelse(A_COHORT$asa5.dummy == 1, 5, 1)) )))

A_COHORT$sexCOMP <- with(A_COHORT, ifelse(A_COHORT$sexm.dummy == 1, 'M', 'F'))

A_COHORT$diabCOMP <- with(A_COHORT, ifelse(A_COHORT$dt1.dummy == 1, '1', ifelse(A_COHORT$dt2.dummy == 1, '2', 'NA')))

A_COHORT$surgeryCOMP <- with(A_COHORT, ifelse(A_COHORT$general.dummy == 1, 'GENERAL', ifelse(A_COHORT$transplant.dummy == 1, 'TRANSPLANT', ifelse(A_COHORT$thoracic.dummy == 1,'THORACIC', ifelse(A_COHORT$otolaryncology.dummy == 1, 'OTOLARYNGOLOGY', ifelse(A_COHORT$neurology.dummy == 1, 'NEUROSURGERY', ifelse(A_COHORT$plastic.dummy == 1, 'PLASTIC', ifelse(A_COHORT$cardio.dummy == 1, 'CARDIO SURGERY', ifelse(A_COHORT$urology.dummy == 1 , 'UROLOGY', ifelse(A_COHORT$vascular.dummy == 1, 'VASCULAR SURGERY', ifelse(A_COHORT$orthopedic.dummy == 1, 'ORTHOPEDIC SURGERY', ifelse(A_COHORT$gynecology.dummy == 1, 'GYNECOLOGY', ifelse(A_COHORT$oral.dummy == 1, 'ORAL SURGERY', 'ANES OFF SITE'))))))))))) ))

A_COHORT$anesCOMP <- with(A_COHORT, ifelse(A_COHORT$intracranial.dummy == 1, 'INTRACRANIAL', ifelse(A_COHORT$cardiac.dummy == 1, 'CPB', ifelse(A_COHORT$liverx.dummy == 1, 'LIVER TX', ifelse(A_COHORT$spinal.dummy == 1, ' SPINAL', 'OTHER')))))

drops <- c('sexm.dummy', 'asa2.dummy', 'asa3.dummy', 'asa4.dummy', 'asa5.dummy', 'dt1.dummy', 'dt2.dummy', 'cardio.dummy', 'general.dummy', 'gynecology.dummy', 'neurology.dummy', 'oral.dummy', 'orthopedic.dummy', 'otolaryncology.dummy', 'plastic.dummy', 'thoracic.dummy', 'transplant.dummy', 'urology.dummy', 'vascular.dummy', 'intracranial.dummy', 'cardiac.dummy', 'liverx.dummy', 'spinal.dummy')
A_COHORT<- A_COHORT[ , !(names(A_COHORT) %in% drops)]

names(A_COHORT)[names(A_COHORT) == 'PeakGData'] <- 'Peak Glucose'
write.xlsx(A_COHORT, 'Oct2020COHORTFINAL.xlsx')

# function to predict glucose
GlucosePredictingFunction <- function (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32){
  hyperglycemia <- predict(appPeakG.xg, as.matrix(data.frame(AgeData = v1, BMIData = v2, PreGData = v3, hbData = v4, Duration = v5, sexm.dummy = v6, asa2.dummy = v7,
                                                             asa3.dummy =  v8, asa4.dummy = v9, asa5.dummy =  v10, iyes.dummy = v11, dt1.dummy = v12, dt2.dummy = v13,
                                                             ohayes.dummy = v14, classin.dummy = v15, emrgyes.dummy = v16, cardio.dummy = v17, general.dummy = v18, gynecology.dummy = v19, 
                                                             neurology.dummy = v20, oral.dummy = v21, orthopedic.dummy = v22, otolaryncology.dummy = v23, plastic.dummy = v24, 
                                                             thoracic.dummy = v25, transplant.dummy = v26, urology.dummy = v27, vascular.dummy = v28, intracranial.dummy = v29, 
                                                             cardiac.dummy = v30, liverx.dummy = v31, spinal.dummy = v32)))
  
  
  hyperglycemia <- toString(round(hyperglycemia,0))
  
  return(hyperglycemia)
}

