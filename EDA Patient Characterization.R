#Akira Nair 

#attaching and importing
attach(RestrictedDataBook) 
attach(EmergencyData)
attach(Extra_Data)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(grid)
library(gridExtra)
library(stringr)
library(car)
library(caret)

#assigns data to a numeric list

WeightData <- WeightORCA
PreGData <- `Last Preoperative Glucose Value`
DMData <- `DM type`
HeightData <- HeightORCA
ASAData <- ASAVAL
AgeData <- Age
PeakGData <- `Peak glucose value`
hbData <- `HbA1C (Last one within 6 mo)`
PriGData <- `1st Intraop glucose value`
IData <- `Preop Insulin`
OHAData <- `Preop OralHyperglycemic Agent`
ClassData <- Pat1entClass
ProblemsData <- PreOperativeProblemList
ProblemsData <- toupper(ProblemsData)
EMRGData <- EMRG
SurgeryData <- AppointmentTypeText
AnesData <- as.numeric(AnesCPT)
SexData <- `Gender`
IHomeData <- InsulinHomeMed
OHAHomeData <- OralHyperglycemicAgentHomeMed
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
hbData[is.na(hbData) & DMData == 0 ] <- 5.0
hbData[is.na(hbData) & DMData == 1 ] <- 6.5
hbData[is.na(hbData) & DMData == 2 ] <- 6.5
PreGData[is.na(PreGData)] <- 110

#calculates BMI from weight and height Data
BMIData = (WeightData)/((HeightData*0.01)^2)
BMIData[BMIData > 60 | BMIData < 5] <- NA


#### Patient Characteristics
s = 4


##Age Component Construction | Table 1
age.bins <- c(0, 17, 65, 98)
agecategories <- cut(AgeData, age.bins, labels = c("<18", "18-65", ">65"))
tblfreq1 <- transform(table(agecategories))
tbl1 <- transform(tblfreq1, Rel_Freq = round(prop.table(Freq), digits=s))
colnames(tbl1) [1] <- "Age"
T1 <- tableGrob(tbl1, rows = NULL)
H1 <- ggplot(RestrictedDataBook, aes(AgeData)) + 
  geom_histogram(bins = 20, col = "coral", fill = "wheat", alpha = 0.7) + xlab("Age (yrs)") + ylab("Freq")

##Duration Component Construction | Table 1.1
H1.1 <- ggplot(RestrictedDataBook, aes(Duration)) + 
  geom_histogram(bins = 20, col = "coral", fill = "wheat", alpha = 0.7) + xlab("Duration (min)") + ylab("Freq")

##Sex Component Construction | Table 1.5
tblfreq1.5 <- transform(table(SexData))
tbl1.5 <- transform(tblfreq1.5, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl1.5) [1] <- "Sex"
T1.5 <- tableGrob(tbl1.5, rows = NULL)
H1.5 <- ggplot(RestrictedDataBook, aes(SexData)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Sex") + ylab("Freq")

##BMI Component Construction | Table 2
bmi.bins <- c(0, 19.99, 29.99, 39.99, 50, 59.99)
bmicategories <- cut(BMIData, bmi.bins, labels = c("<20", "20-30", "30-40", "40-50", ">50"))
tblfreq2 <- transform(table(bmicategories))
tbl2 <- transform(tblfreq2, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl2) [1] <- "BMI"
T2 <- tableGrob(tbl2, rows = NULL)
H2 <- ggplot(RestrictedDataBook, aes(BMIData)) + 
  geom_histogram(bins = 15, col = "coral", fill = "wheat", alpha = 0.7) + xlab("BMI") + ylab("Freq")

##ASA Component Construction | Table 3
asa.bins <- c(0, 1, 2, 3, 4, 5)
asacategories <- cut(ASAData, asa.bins, labels = c("1", "2", "3", "4", "5"))
tblfreq3 <- transform(table(asacategories))
tbl3 <- transform(tblfreq3, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl3) [1] <- "ASA"
T3 <- tableGrob(tbl3, rows = NULL)
H3 <- ggplot(RestrictedDataBook, aes(ASAData)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("ASA") + ylab("Freq")

#### Glucose Characteristics

##HbA1c Component Construction | Table 4
hb.bins <- c(0, 5.99, 6.49, 7, 14)
hbcategories <- cut(hbData, hb.bins, labels = c("<6.0", "6.0-6.5", "6.5-7.0", ">7.0"))
tblfreq4 <- transform(table(hbcategories))
tbl4 <- transform(tblfreq4, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl4) [1] <- "HbA1c"
T4 <- tableGrob(tbl4, rows = NULL)
H4 <- ggplot(RestrictedDataBook, aes(hbData)) + 
  geom_histogram(bins = 10, col = "coral", fill = "wheat", alpha = 0.7) + xlab("HbA1c (mmol/mol)") + ylab("Freq")

##PreOp Glucose Component Construction | Table 5
pop.bins <- c(0, 99.99, 139.99, 180, 250)
popcategories <- cut(PreGData, pop.bins, labels = c("<100", "100-140", "140-180", ">180"))
tblfreq5 <- transform(table(popcategories))
tbl5 <- transform(tblfreq5, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl5) [1] <- "Preop Glucose"
T5 <- tableGrob(tbl5, rows = NULL)
H5 <- ggplot(RestrictedDataBook, aes(PreGData)) + 
  geom_histogram(bins = 15, col = "coral", fill = "wheat", alpha = 0.7) + xlab("Preop Glucose (mg/dl)") + ylab("Freq")

##PeakOp Glucose Component Construction | Table 6
iop.bins <- c(0, 99.99, 139.99, 180, 250)
iopcategories <- cut(PeakGData, iop.bins, labels = c("<100", "100-140", "140-180", ">180"))
tblfreq6 <- transform(table(iopcategories))
tbl6 <- transform(tblfreq6, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl6) [1] <- "Peak Glucose"
T6 <- tableGrob(tbl6, rows = NULL)
H6 <- ggplot(RestrictedDataBook, aes(PeakGData)) + 
  geom_histogram(bins = 15, col = "coral", fill = "wheat", alpha = 0.7) + xlab("Peak Glucose (mg/dl)") + ylab("Freq")

##1st IntraOp Glucose Component Construction | Table 7
fop.bins <- c(0, 99.99, 139.99, 180, 250)
fopcategories <- cut(PriGData, fop.bins, labels = c("<100", "100-140", "140-180", ">180"))
tblfreq7 <- transform(table(fopcategories))
tbl7 <- transform(tblfreq7, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl7) [1] <- "Intraop Glucose"
T7 <- tableGrob(tbl7, rows = NULL)
H7 <- ggplot(RestrictedDataBook, aes(PriGData)) + 
  geom_histogram(bins = 15, col = "coral", fill = "wheat", alpha = 0.7) + xlab("Intraop Glucose (mg/dl)") + ylab("Freq")

##Insulin Component Construction | Table 8
icategories <- cut(IData, 2, labels = c("NO", "YES"))
tblfreq8 <- transform(table(icategories))
tbl8 <- transform(tblfreq8, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl8) [1] <- "Insulin"
T8 <- tableGrob(tbl8, rows = NULL)
H8 <- ggplot(RestrictedDataBook, aes(icategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Insulin") + ylab("Freq")

##DT Component Construction | Table 9
dtcategories <- cut(DMData, 3, labels = c("UD", "1", "2"))
tblfreq9 <- transform(table(dtcategories))
tbl9 <- transform(tblfreq9, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl9) [1] <- "DT"
T9 <- tableGrob(tbl9, rows = NULL)
H9 <- ggplot(RestrictedDataBook, aes(dtcategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("DM") + ylab("Freq")

##OHA Component Construction | Table 10
ohacategories <- cut(OHAData, 2, labels = c("No", "Yes"))
tblfreq10 <- transform(table(ohacategories))
tbl10 <- transform(tblfreq10, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl10) [1] <- "OHA"
T10 <- tableGrob(tbl10, rows = NULL)
H10 <- ggplot(RestrictedDataBook, aes(ohacategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("OHA") + ylab("Freq")


####Procedure Characteristics

##Class Component Construction | Table 11
ccategories <- cut(ClassData, 2, labels = c("OUT", "IN"))
tblfreq11 <- transform(table(ccategories))
tbl11 <- transform(tblfreq11, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl11) [1] <- "Patient Class"
T11 <- tableGrob(tbl11, rows = NULL)
H11 <- ggplot(RestrictedDataBook, aes(ccategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Patient Class") + ylab("Freq")


##Medical Problem Component Construction | Table 12-18

#Pulmonary | Table 12
x = 1
pulmonary = list() #will end up being list of 1s and 0s
while (x <= length (ProblemsData)){ #X is row number; goes through each row in the problems column
  ii = as.numeric(grepl("PULMONARY", ProblemsData[x])) #outputs 1 if word is in cell, 0 if not
  pulmonary[x] <- ii #adds the 1 or 0 to the list
  x=x+1
}
pulmonary <- as.numeric(pulmonary)
pulmonarycategories <- cut(pulmonary, 2, labels = c("No pulmonary", "Yes pulmonary"))
tblfreq12 <- transform(table(pulmonarycategories))
tbl12 <- transform(tblfreq12, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl12) [1] <- "Pulmonary"
T12 <- tableGrob(tbl12, rows = NULL)
H12 <- ggplot(RestrictedDataBook, aes(pulmonarycategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Pulmonary") + ylab("Freq")

#Cancer | Table 13
x = 1
cancer = list()
while (x <= length (ProblemsData)){
  ii = as.numeric(grepl("CANCER", ProblemsData[x]))
  cancer[x] <- ii
  x=x+1
}
cancer <- as.numeric(cancer)
cancercategories <- cut(cancer, 2, labels = c("No cancer", "Yes cancer"))
tblfreq13 <- transform(table(cancercategories))
tbl13 <- transform(tblfreq13, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl13) [1] <- "Cancer"
T13 <- tableGrob(tbl13, rows = NULL)
H13 <- ggplot(RestrictedDataBook, aes(cancercategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Cancer") + ylab("Freq")

#Cardiac | Table 14
x = 1
cardiac = list()
while (x <= length (ProblemsData)){
  iia = as.numeric(grepl("HYPERTENSION", ProblemsData[x]))
  iib = as.numeric(grepl("CORONARY", ProblemsData[x]))
  iic = as.numeric(grepl("HEART FAILURE", ProblemsData[x]))
  iid = as.numeric(grepl("VALVE DISEASE", ProblemsData[x]))
  if (iia == 1 | iib == 1 | iic == 1 | iid == 1){
    cardiac[x] <-1
  }
  else {
    cardiac[x] <-0
  }

  x=x+1
}
cardiac <- as.numeric(cardiac)
cardiaccategories <- cut(cardiac, 2, labels = c("No cardiac", "Yes cardiac"))
tblfreq14 <- transform(table(cardiaccategories))
tbl14 <- transform(tblfreq14, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl14) [1] <- "Cardiac"
T14 <- tableGrob(tbl14, rows = NULL)
H14 <- ggplot(RestrictedDataBook, aes(cardiaccategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Cardiac") + ylab("Freq")

#Renal | Table 15
x = 1
renal = list()
while (x <= length (ProblemsData)){
  ii = as.numeric(grepl("RENAL", ProblemsData[x]))
  renal[x] <- ii
  x=x+1
}
renal <- as.numeric(renal)
renalcategories <- cut(renal, 2, labels = c("No renal", "Yes renal"))
tblfreq15 <- transform(table(renalcategories))
tbl15 <- transform(tblfreq15, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl15) [1] <- "Renal"
T15 <- tableGrob(tbl15, rows = NULL)
H15 <- ggplot(RestrictedDataBook, aes(renalcategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Renal") + ylab("Freq")

#Hepatic | Table 16
x = 1
hepatic = list()
while (x <= length (ProblemsData)){
  ii = as.numeric(grepl("HEPATIC", ProblemsData[x]))
  hepatic[x] <- ii
  x=x+1
}
hepatic <- as.numeric(hepatic)
hepaticcategories <- cut(hepatic, 2, labels = c("No hepatic", "Yes hepatic"))
tblfreq16 <- transform(table(hepaticcategories))
tbl16 <- transform(tblfreq16, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl16) [1] <- "Hepatic"
T16 <- tableGrob(tbl16, rows = NULL)
H16 <- ggplot(RestrictedDataBook, aes(hepaticcategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Hepatic") + ylab("Freq")

#Digestive | Table 17
x = 1
gerd = list()
while (x <= length (ProblemsData)){
  ii = as.numeric(grepl("GERD", ProblemsData[x]))
  gerd[x] <- ii
  x=x+1
}
gerd <- as.numeric(gerd)
gerdcategories <- cut(gerd, 2, labels = c("No GERD", "Yes GERD"))
tblfreq17 <- transform(table(gerdcategories))
tbl17 <- transform(tblfreq17, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl17) [1] <- "Gerd"
T17 <- tableGrob(tbl17, rows = NULL)
H17 <- ggplot(RestrictedDataBook, aes(gerdcategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("GERD") + ylab("Freq")

#Smoking | Table 18
x = 1
smoking = list()
while (x <= length (ProblemsData)){
  ii = as.numeric(grepl("SMOKING", ProblemsData[x]))
  smoking[x] <- ii
  x=x+1
}
smoking <- as.numeric(smoking)
smokingcategories <- cut(smoking, 2, labels = c("No smoking", "Yes smoking"))
tblfreq18 <- transform(table(smokingcategories))
tbl18 <- transform(tblfreq18, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl18) [1] <- "Smoking"
T18 <- tableGrob(tbl18, rows = NULL)
H18 <- ggplot(RestrictedDataBook, aes(smokingcategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Smoking") + ylab("Freq")

##Emergency Component Construction | Table 19
emrgcategories <- cut(EMRGData, 2, labels = c("No EMRG", "EMRG"))
tblfreq19 <- transform(table(emrgcategories))
tbl19 <- transform(tblfreq19, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl19) [1] <- "Emergency"
T19 <- tableGrob(tbl19, rows = NULL)
H19 <- ggplot(RestrictedDataBook, aes(emrgcategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Emergency") + ylab("Freq")

##Surgery Component Construction | Table 20
tblfreq20 <- transform(table(SurgeryData))
tbl20 <- transform(tblfreq20, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl20) [1] <- "Surgery Type"
T20 <- tableGrob(tbl20, rows = NULL)
H20 <- ggplot(RestrictedDataBook, aes(SurgeryData)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Surgery Type") + ylab("Freq") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

##AnesCPT Component Construction | Table 21
AnesData[210 <= AnesData & AnesData <= 222] <- 1 #intracranial
AnesData[560 <= AnesData & AnesData <= 580] <- 2 #cardiac
AnesData[AnesData==796] <- 3 #liverx
AnesData[600 <= AnesData & AnesData <= 670] <- 4 #spinal
AnesData[AnesData!= 1 & AnesData!= 2 & AnesData!= 3 & AnesData!= 4 | is.na(AnesData)] <- 0
anescategories <- cut(AnesData, 5, labels = c("Other", "Intracranial", "Cardiac", "Liver X", "Spinal"))
tblfreq21 <- transform(table(anescategories))
tbl21 <- transform(tblfreq21, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl21) [1] <- "Anesthesia Type"
T21 <- tableGrob(tbl21, rows = NULL)
H21 <- ggplot(RestrictedDataBook, aes(anescategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Anesthesia Type") + ylab("Freq") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

##Insulin Home Med Component Construction | Table 22
ihomecategories <- cut(IHomeData, 2, labels = c("No", "Yes"))
tblfreq22 <- transform(table(ihomecategories))
tbl22 <- transform(tblfreq22, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl22) [1] <- "Insulin as Home Med"
T22 <- tableGrob(tbl22, rows = NULL)
H22 <- ggplot(Extra_Data, aes(ihomecategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("Insulin as Home Med") + ylab("Freq") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

##OHA Home Med Component Construction | Table 23
ohahomecategories <- cut(OHAHomeData, 2, labels = c("No", "Yes"))
tblfreq23 <- transform(table(ohahomecategories))
tbl23 <- transform(tblfreq23, Rel_Freq = round(prop.table(Freq), digits = s))
colnames(tbl23) [1] <- "OHA as Home Med"
T23 <- tableGrob(tbl23, rows = NULL)
H23 <- ggplot(Extra_Data, aes(ohahomecategories)) + geom_bar(col = "coral", fill = "wheat", alpha = 0.7) + xlab("OHA as Home Med") + ylab("Freq") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


####PLOTTING


#Age vs Glucose | Graph 1
test_data <- data.frame(`Peak Glucose` = PeakGData, `Preop Glucose` = PreGData, `First Introp Glucose` = PriGData, AgeData)
test_data_long <- melt(test_data, id="AgeData")  # convert to long format
graph1 <-ggplot(data=test_data_long, aes(x=AgeData, y=value, colour=variable)) + geom_smooth() + ggtitle("Age vs. Glucose") + xlab("Age (yrs)") + ylab("Glucose Level (mg/dl)")

#Duration vs Glucose | Graph 1.1
test_data <- data.frame(`Peak Glucose` = PeakGData, `Preop Glucose` = PreGData, `First Introp Glucose` = PriGData, Duration)
test_data_long <- melt(test_data, id="Duration")  # convert to long format
graph1.1 <-ggplot(data=test_data_long, aes(x=Duration, y=value, colour=variable)) + geom_smooth() + ggtitle("Duration vs. Glucose") + xlab("Duration (min)") + ylab("Glucose Level (mg/dl)")

#Sex vs Glucose | Graph 1.5
sexdf <- data.frame(SexData, PeakGData, PreGData, PriGData)
sexdf <- sexdf[complete.cases(sexdf),]
graph1.5a <-ggplot(sexdf, aes(x = sexdf[,1], y = sexdf[,2], group = sexdf[,1])) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("Sex vs. Peak Glucose") + xlab("Sex") + ylab("Glucose Level (mg/dl)")
graph1.5b <-ggplot(sexdf, aes(x = sexdf[,1], y = sexdf[,3], group = sexdf[,1])) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("Sex vs. Preop Glucose") + xlab("Sex") + ylab("Glucose Level (mg/dl)")
graph1.5c <-ggplot(sexdf, aes(x = sexdf[,1], y = sexdf[,4], group = sexdf[,1])) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("Sex vs. Intraop Glucose") + xlab("Sex") + ylab("Glucose Level (mg/dl)")

#BMI vs Glucose | Graph 2
test_data <- data.frame(`Peak Glucose` = PeakGData, `Preop Glucose` = PreGData, `First Introp Glucose` = PriGData, BMIData)
test_data_long <- melt(test_data, id="BMIData")  # convert to long format
graph2 <-ggplot(data=test_data_long, aes(x=BMIData, y=value, colour=variable)) + geom_smooth() + ggtitle("BMI vs. Glucose") + xlab("BMI") + ylab("Glucose Level (mg/dl)")

#ASA vs Glucose | Graph 3
graph3a <-ggplot(RestrictedDataBook, aes(x = ASAData, y = PeakGData, group = ASAData)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("ASA vs. Peak Glucose") + xlab("ASA") + ylab("Glucose Level (mg/dl)")
graph3b <-ggplot(RestrictedDataBook, aes(x = ASAData, y = PreGData, group = ASAData)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("ASA vs. Preop Glucose") + xlab("ASA") + ylab("Glucose Level (mg/dl)")
graph3c <-ggplot(RestrictedDataBook, aes(x = ASAData, y = PriGData, group = ASAData)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("ASA vs. Intraop Glucose") + xlab("ASA") + ylab("Glucose Level (mg/dl)")

#HbA1C vs Glucose | Graph 4
test_data <- data.frame(`Peak Glucose` = PeakGData, `Preop Glucose` = PreGData, `First Introp Glucose` = PriGData, hbData)
test_data_long <- melt(test_data, id="hbData")  # convert to long format
graph4 <-ggplot(data=test_data_long, aes(x=hbData, y=value, colour=variable)) + geom_smooth() + ggtitle("HbA1c vs. Glucose") + xlab("HbA1c (mmol/mol)") + ylab("Glucose Level (mg/dl)")

#DT vs Glucose | Graph 5
graph5a <-ggplot(RestrictedDataBook, aes(x = dtcategories, y = PeakGData, group = dtcategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("DT vs. Peak Glucose") + xlab("DT") + ylab("Glucose Level (mg/dl)")
graph5b <-ggplot(RestrictedDataBook, aes(x = dtcategories, y = PreGData, group = dtcategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("DT vs. Preop Glucose") + xlab("DT") + ylab("Glucose Level (mg/dl)")
graph5c <-ggplot(RestrictedDataBook, aes(x = dtcategories, y = PriGData, group = dtcategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("DT vs. Intraop Glucose") + xlab("DT") + ylab("Glucose Level (mg/dl)")

#Pulmonary vs Glucose | Graph 6
graph6a <-ggplot(RestrictedDataBook, aes(x = pulmonarycategories, y = PeakGData, group = pulmonarycategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("Pulmonary vs. Peak Glucose") + xlab("Pulmonary") + ylab("Glucose Level (mg/dl)")
graph6b <-ggplot(RestrictedDataBook, aes(x = pulmonarycategories, y = PreGData, group = pulmonarycategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("Pulmonary vs. Preop Glucose") + xlab("Pulmonary") + ylab("Glucose Level (mg/dl)")
graph6c <-ggplot(RestrictedDataBook, aes(x = pulmonarycategories, y = PriGData, group = pulmonarycategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("Pulmonary vs. Intraop Glucose") + xlab("Pulmonary") + ylab("Glucose Level (mg/dl)")

#Cancer vs Glucose | Graph 7
graph7a <-ggplot(RestrictedDataBook, aes(x = cancercategories, y = PeakGData, group = cancercategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("Cancer vs. Peak Glucose") + xlab("Cancer") + ylab("Glucose Level (mg/dl)")
graph7b <-ggplot(RestrictedDataBook, aes(x = cancercategories, y = PreGData, group = cancercategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("Cancer vs. Preop Glucose") + xlab("Cancer") + ylab("Glucose Level (mg/dl)")
graph7c <-ggplot(RestrictedDataBook, aes(x = cancercategories, y = PriGData, group = cancercategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("Cancer vs. Intraop Glucose") + xlab("Cancer") + ylab("Glucose Level (mg/dl)")

#Cardiac vs Glucose | Graph 8
graph8a <-ggplot(RestrictedDataBook, aes(x = cardiaccategories, y = PeakGData, group = cardiaccategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("Cardiac vs. Peak Glucose") + xlab("Cardiac") + ylab("Glucose Level (mg/dl)")
graph8b <-ggplot(RestrictedDataBook, aes(x = cardiaccategories, y = PreGData, group = cardiaccategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("Cardiac vs. Preop Glucose") + xlab("Cardiac") + ylab("Glucose Level (mg/dl)")
graph8c <-ggplot(RestrictedDataBook, aes(x = cardiaccategories, y = PriGData, group = cardiaccategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("Cardiac vs. Intraop Glucose") + xlab("Cardiac") + ylab("Glucose Level (mg/dl)")

#Renal vs Glucose | Graph 9
graph9a <-ggplot(RestrictedDataBook, aes(x = renalcategories, y = PeakGData, group = renalcategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("Renal vs. Peak Glucose") + xlab("Renal") + ylab("Glucose Level (mg/dl)")
graph9b <-ggplot(RestrictedDataBook, aes(x = renalcategories, y = PreGData, group = renalcategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("Renal vs. Preop Glucose") + xlab("Renal") + ylab("Glucose Level (mg/dl)")
graph9c <-ggplot(RestrictedDataBook, aes(x = renalcategories, y = PriGData, group = renalcategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("Renal vs. Intraop Glucose") + xlab("Renal") + ylab("Glucose Level (mg/dl)")

#Hepatic vs Glucose | Graph 10
graph10a <-ggplot(RestrictedDataBook, aes(x = hepaticcategories, y = PeakGData, group = hepaticcategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("Hepatic vs. Peak Glucose") + xlab("Hepatic") + ylab("Glucose Level (mg/dl)")
graph10b <-ggplot(RestrictedDataBook, aes(x = hepaticcategories, y = PreGData, group = hepaticcategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("Hepatic vs. Preop Glucose") + xlab("Hepatic") + ylab("Glucose Level (mg/dl)")
graph10c <-ggplot(RestrictedDataBook, aes(x = hepaticcategories, y = PriGData, group = hepaticcategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("Hepatic vs. Intraop Glucose") + xlab("Hepatic") + ylab("Glucose Level (mg/dl)")

#Digestive vs Glucose | Graph 11
graph11a <-ggplot(RestrictedDataBook, aes(x = gerdcategories, y = PeakGData, group = gerdcategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("GERD vs. Peak Glucose") + xlab("GERD") + ylab("Glucose Level (mg/dl)")
graph11b <-ggplot(RestrictedDataBook, aes(x = gerdcategories, y = PreGData, group = gerdcategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("GERD vs. Preap Glucose") + xlab("GERD") + ylab("Glucose Level (mg/dl)")
graph11c <-ggplot(RestrictedDataBook, aes(x = gerdcategories, y = PriGData, group = gerdcategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("GERD vs. Intraop Glucose") + xlab("GERD") + ylab("Glucose Level (mg/dl)")

#Smoking vs Glucose | Graph 12
graph12a <-ggplot(RestrictedDataBook, aes(x = smokingcategories, y = PeakGData, group = smokingcategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("Smoking vs. Peak Glucose") + xlab("Smoking") + ylab("Glucose Level (mg/dl)")
graph12b <-ggplot(RestrictedDataBook, aes(x = smokingcategories, y = PreGData, group = smokingcategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("Smoking vs. Preop Glucose") + xlab("Smoking") + ylab("Glucose Level (mg/dl)")
graph12c <-ggplot(RestrictedDataBook, aes(x = smokingcategories, y = PriGData, group = smokingcategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("Smoking vs. Intraop Glucose") + xlab("Smoking") + ylab("Glucose Level (mg/dl)")

#Anes vs Glucose | Graph 13
graph13a <-ggplot(RestrictedDataBook, aes(x = anescategories, y = PeakGData, group = anescategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("AnesCPT vs. Peak Glucose") + xlab("Anes Type") + ylab("Glucose Level (mg/dl)")
graph13b <-ggplot(RestrictedDataBook, aes(x = anescategories, y = PreGData, group = anescategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("AnesCPT vs. Preop Glucose") + xlab("Anes Type") + ylab("Glucose Level (mg/dl)")
graph13c <-ggplot(RestrictedDataBook, aes(x = anescategories, y = PriGData, group = anescategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("AnesCPT vs. Intraop Glucose") + xlab("Anes Type") + ylab("Glucose Level (mg/dl)")

#Surgery vs Glucose | Graph 14
graph14a <-ggplot(RestrictedDataBook, aes(x = SurgeryData, y = PeakGData, group = SurgeryData)) + geom_boxplot(fill = "red", width = 0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Surgery Type vs. Peak Glucose") + xlab("Surgery Type") + ylab("Glucose Level (mg/dl)")
graph14b <-ggplot(RestrictedDataBook, aes(x = SurgeryData, y = PreGData, group = SurgeryData)) + geom_boxplot(fill = "green", width = 0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Surgery Type vs. Preop Glucose") + xlab("Surgery Type") + ylab("Glucose Level (mg/dl)")
graph14c <-ggplot(RestrictedDataBook, aes(x = SurgeryData, y = PeakGData, group = SurgeryData)) + geom_boxplot(fill = "blue", width = 0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Surgery Type vs. Intraop Glucose") + xlab("Surgery Type") + ylab("Glucose Level (mg/dl)")

#Insulin Home Med vs Glucose | Graph 15
graph15a <-ggplot(RestrictedDataBook, aes(x = ihomecategories, y = PeakGData, group = ihomecategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("Insulin Home Med vs. Peak Glucose") + xlab("Insulin Home Med") + ylab("Glucose Level (mg/dl)")
graph15b <-ggplot(RestrictedDataBook, aes(x = ihomecategories, y = PreGData, group = ihomecategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("Insulin Home Med vs. Preop Glucose") + xlab("Insulin Home Med") + ylab("Glucose Level (mg/dl)")
graph15c <-ggplot(RestrictedDataBook, aes(x = ihomecategories, y = PriGData, group = ihomecategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("Insulin Home Med vs. Intraop Glucose") + xlab("Insulin Home Med") + ylab("Glucose Level (mg/dl)")

#OHA Home Med vs Glucose | Graph 16
graph16a <-ggplot(RestrictedDataBook, aes(x = ohahomecategories, y = PeakGData, group = ohahomeanescategories)) + geom_boxplot(fill = "red", width = 0.5) + ggtitle("OHA Home Med vs. Peak Glucose") + xlab("OHA Home Med") + ylab("Glucose Level (mg/dl)")
graph16b <-ggplot(RestrictedDataBook, aes(x = ohahomecategories, y = PreGData, group = ohahomeanescategories)) + geom_boxplot(fill = "green", width = 0.5) + ggtitle("OHA Home Med vs. Preop Glucose") + xlab("OHA Home Med") + ylab("Glucose Level (mg/dl)")
graph16c <-ggplot(RestrictedDataBook, aes(x = ohahomecategories, y = PriGData, group = ohahomeanescategories)) + geom_boxplot(fill = "blue", width = 0.5) + ggtitle("OHA Home Med vs. Intraop Glucose") + xlab("OHA Home Med") + ylab("Glucose Level (mg/dl)")



###~~~~~~


#### OUTPUT



## INFORMATION

## Numeric Variables

modefunc <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
numvar = data.frame(AgeData, BMIData, hbData, Duration, PreGData, PriGData, PeakGData)
numvar = numvar[complete.cases(numvar),]
xnlabels = c("Age", "BMI", "HbA1c", "Duration","Preop", "Intraop", "Peak")
i = 1
while (i <= length(numvar[1,])){
  print(xnlabels[i])
  outliertest<-boxplot(numvar[, i])
  cat("Minimum:", min(na.omit(numvar[, i] )), " \n")
  cat("Maximum:", max(na.omit(numvar[, i] )), " \n")
  cat("Missing Values:", sum(is.na(numvar[, i] )), " \n")
  cat("# of Outliers:", length(outliertest$out), " \n")
  cat("Mean:", mean(na.omit(numvar[, i] )), " \n")
  cat("Median:", median(na.omit(numvar[, i] )), " \n")
  cat("Mode: ", modefunc(na.omit(numvar[, i] )), " \n")
  cat("Standard Deviation:", sd(na.omit(numvar[, i] )), " \n")
  quants<-quantile(na.omit(numvar[,i]), probs = c(0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99))
  cat("Quantiles:", quants, " \n")
  normdata <- na.omit(numvar[, i])
  normality <-shapiro.test(normdata[1:5000])
  cat("Normality:", "W=", normality$statistic, "p-value=", normality$p.value, "\n\n")

  i = i+1
}
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


numvar = data.frame(AgeData, BMIData, hbData, Duration, PreGData, PriGData, PeakGData,sexm.dummy, asa2.dummy, asa3.dummy, asa4.dummy, asa5.dummy, iyes.dummy, 
                    dt1.dummy, dt2.dummy, ohayes.dummy, classin.dummy, emrgyes.dummy, 
                    cardio.dummy, general.dummy, gynecology.dummy, neurology.dummy,
                    oral.dummy, orthopedic.dummy, otolaryncology.dummy, plastic.dummy,
                    thoracic.dummy, transplant.dummy, urology.dummy, vascular.dummy,
                    intracranial.dummy, cardiac.dummy, liverx.dummy, spinal.dummy)
numvar = numvar[complete.cases(numvar),]
for (i in 1:ncol(numvar)){
  numvar[,i] <- as.numeric(numvar[,i])
}
numcor <- cor(numvar)
colnames(numcor) <- c(1:ncol(numvar))
rownames(numcor) <- c(1:ncol(numvar))
corrplot(numcor, method = "circle", type = "lower")
# catvar <- data.frame()
# catvar = catvar[complete.cases(catvar),]
# catcor <- cor(catvar)
# colnames(catcor) <- c("Male", "ASA 2", "ASA 3", "ASA 4", "ASA 5", "Insulin Yes", "DT 1", "DT 2",
#                       "OHA Yes", "Class In", "Emergency Yes", "Cardio", "General", "Gynecology", "Neurology",
#                       "Oral", "Orthopedic", "Otolaryngology", "Plastic", "Thoracic", "Transplant", "Urology",
#                       "Vascular", "Anes.Intracranial", "Anes.Cardiac", "Anes.Liver X", "Anes.Spinal")
# colnames(catcor) <- c(1:27)
# rownames(catcor) <- c("1", "2", "3", "4", "5", "6", "7","8","9","10","11","12","13","14","15","16","17",
#                       "18","19","20","21","22","23","24","25","26","27")
# corrplot(catcor, method = "circle", type = "upper")

# ## Bivariate Numeric
i=1
bivariates <- combn(colnames(numvar), 2)
while (i <= ncol(bivariates)){
  correlation<-cor(numvar[bivariates[1, i]], numvar[bivariates[2, i]], use = 'complete.obs')
  cat(bivariates[1,i], "vs.", bivariates[2,i], "\n")
  cat("Correlation:", correlation, "\n\n")
  i=i+1
}

# ## Bivariate Categorical

catvar = data.frame(SexData, ASAData, IData, DMData, OHAData, ClassData, emrgcategories, SurgeryData, anescategories)
i=1
bivariates2 <- combn(colnames(catvar), 2)
s = 2
while (i <= ncol(bivariates2)){
  dummydf <- data.frame(catvar[,bivariates2[1,i]], catvar[,bivariates2[2,i]])
  dummydf <- dummydf[complete.cases(dummydf),]
  dummy1 <- model.matrix(~factor(dummydf[,1]))
  dummy2 <- model.matrix(~factor(dummydf[,2]))
  k=2
  while (k<= ncol(dummy1)){
    kk=2
    while (kk <= ncol(dummy2)){
      association <- round(cor(dummy1[,k], dummy2[,kk]), digits = s)
      varcode1 <- (k-1)
      varcode2 <- (kk-1)
      cat("Association between ", bivariates2[1,i], "-", varcode1 ,"and", bivariates2[2,i], "-", varcode2, "\n", association, "\n\n")
      kk = kk+1
    }
   k = k+1
  }

  i=i+1
}

# ## Bivariate Numeric vs Categorical

i = 1 #catvar id
while (i <= ncol(catvar)){ #goes through each categorical variable
  ii=1 #numvar id
  while (ii <= ncol(numvar)){ # for each categorical variable go through each numerical variable
    k=2 #dummy id
    dummydf <- data.frame(catvar[,i], numvar[,ii])
    dummydf <- dummydf[complete.cases(dummydf),] #complete cases between categorical variable and numerical variable
    dummycat <- model.matrix(~factor(dummydf[,1])) #create dummies for the categorical variable
    while (k <= ncol(dummycat)){ #ncol dummycat is how many dummies were created for the categorical variable; for each dummy we find correlation
      cordf <- data.frame(dummycat[,k], dummydf[,2])
      cordf <- cordf[complete.cases(cordf),] # match dimensions
      correlation <- round(cor(cordf[,1], cordf[,2]), digits = s)
      cat("Correlation between ", colnames(catvar)[i], "-", k-1, "and", colnames(numvar[ii]), "\n",correlation, "\n")
      k = k+1
    }
    ii = ii+1
  }
  i = i+1
}






# ###Table and Histogram Outputs
# plot(T1); H1; H1.1; plot(T2); H2; plot(T3); H3; plot(T4); H4; plot(T5); H5; plot(T6); H6;
# plot(T7); H7; plot(T8); H8; plot(T9); H9; plot(T10); H10; plot(T11); H11; plot(T12); H12;
# plot(T13); H13; plot(T14); H14; plot(T15); H15; plot(T16); H16; plot(T17); H17; plot(T18);
# H18; plot(T19); H19; plot(T20); H20; plot(T21); H21; plot(T22); H22; plot(T23); H23
# 
# 
# ###Graphical Outputs
# graph1; graph1.1; graph1.5a; graph1.5b; graph1.5c; graph2; graph3a; graph3b; graph3c; graph4; graph5a;
# graph5b; graph5c; graph6a; graph6b; graph6c; graph7a; graph7b; graph7c; graph8a; graph8b; graph8c;
# graph9a; graph9b; graph9c; graph10a; graph10b; graph10c; graph11a; graph11b; graph12c; graph13a;
# graph13b; graph13c; graph14a; graph14b; graph14c; graph15a; graph15b; graph15c; graph16a; graph16b; graph16c
# 
# 
 #### Scatterplot creator
 xnset = data.frame(AgeData, BMIData, hbData, PreGData, PriGData)
 xnlabels = c("Age", "BMI", "HbA1c", "Preop", "Intraop")
 iii = 1
 while (iii <= length(xnset)) {
   data <- xnset[,iii]
   splot <- ggplot(data= xnset, aes(x=data, y=PeakGData)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + xlab(xnlabels[iii]) + ylab("Peak Glucose")
   print(splot)
   iii = iii+1
 }
 
# #### Mosaic creator
# coli = length(unique(na.exclude(iopcategories)))+1
# xnset2 = data.frame(ASAData, dtcategories, pulmonarycategories, cancercategories,
#                     cardiaccategories, renalcategories, hepaticcategories, gerdcategories,
#                     smokingcategories, anescategories, SurgeryData, ihomecategories, ohahomecategories)
# xnlabels2 = c("ASA", "DM", "Pulmonary", "Cancer", "Cardiac", "Renal", "Hepatic", "Gerd",
#              "Smoking", "Anesthesia Type", "Surgery Type", "Insulin Home Med", "OHA Home Med")
# iii = 1
# while (iii <= length(xnset2)) {
#   data2 <- xnset2[,iii]
#   total = table(data2, iopcategories)
#   mplot <- mosaicplot(total, col=(coli:2), xlab = xnlabels2[iii], ylab = "Peak Glucose", main = "", las = 2)
#   print(mplot)
#   iii = iii+1
# }

 


 


####END EDA










