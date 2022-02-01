##### Akira Nair, 2019
##### PERIMATICS OPIOID PREDICTIVE MODELLING

## The Preliminary Data Analysis consists of the missing values/inputation, central tendencies, and the variable creation steps completed prior to EDA

library(readxl)
library(plyr)
library(tm)
library(wordcloud)
library(ggplot2)
library(stringr)
library(fastDummies)
library(corrplot)
library(MLmetrics)
library(xgboost)
library(caret)
library(e1071)
library(nnet)
library(neuralnet)
library(ridge)
library(readxl)
PostopOpioidDataSheet <- read_excel("~/Documents/Opioid Predictive Model/08-14 PostopOpioid.xlsx")
attach(PostopOpioidDataSheet)

# storing data into variables
#Patient Information
AgeData <- AGE #categorical or numerical?
SexData <- GENDER
RaceData <- Race
WeightData <- `ORCA Weight (kg)` # only 20 instances of weight greater than 500 lbs, but 358 instances of weight less than 50, 351 of them are 0s
HeightData <- `ORCA Height (cm)`
ASAData <- `ASA status`
#Surgery Information
SurgeryTypeData <- `Surgery Type`
SurgeryDescData <- `Surgery description`
AnesCPTData <- `ANES CPT`
GAData <- `GA Type`
TIVAData <- `TIVA - Y/N`
ProcDurationData <- `PROC DURATION (min)`
AnesDurationData <- `ANES DURATION (min)`
#Agents Administered
SevofluraneData <- `SEVOFLURANE (min)`
DesfluraneData <- `DESFLURANE (min)`
IsofluraneData <- `ISOFLURANE (min)`
FentanylData <- `FENTANYL (mcg)` ##
HydromorphoneData <- `HYDROMORPHONE (mg)` ##
MorphineData <- `MORPHINE (mg)` ##
RemifentanilData <- `REMIFENTANIL (mg)` ##
KetamineData <- `KETAMINE (mg)`
KetorolacData <- `KETOROLAC (mg)`
AcetaminophenData <- `ACETAMINOPHEN (mg)`
LidocaineData <- `LIDOCAINE INFUSION`
PropofolData <- `PROPOFOL TOTAL (mg)`
PropofolInfusionData <- `Propofol Infusion rate (mcg/kg/min)`
PropofolDurationData <- `Propofol infusion duration (min)`
LocalData <- `LOCAL BY SURGEON`
CrystalloidsData <- CRYSTALLOIDS
ColloidsData <- COLLOIDS
BloodData <- `BLOOD PRODUCTS`
UrineData <- URINE
EBLData <- EBL
PositionData <- POSITION
EsmololData <- `ESMOLOL INFUSION`
DexmedetomidineData <- `DEXMEDETOMIDINE INFUSION`
MethadoneData <- METHADONE
AlfentanilData <- ALFENTANIL
SufentanilData <- SUFENTANIL
NaloxoneData <- `NALOXONE (mg)`
#Preoperative
PreMorphineData <- `PreOP Morphine`
PreFentanylData <- `PreOp Fentanyl`
PreHydromophoneData <- `PreOp Hydromorphone`
PreMethadoneData <- `PreOp Methadone`
PreTramadolData <- `PreOp Tramadol`
PreOxycodoneData <- `PreOp Oxycodone`
PreMeperidineData <- `PreOp Meperidine`
PreAcetaminophenData <- `PreOp Acetaminophen`
PreGabapentinData <- `PreOp Gabapentin`
PreCelecoxibData <- `PreOp Celecoxib`
PrePainScoreData <- `Preop Pain Score`
PrePainLevelData <- `PreOp Pain Level`
#Postoperative
PostMorphineData <- `PostOp Morphine`
PostFentanylData <- `PostOp Fentanyl`
PostHydromorphoneData <- `PostOp Hydromorphone`
PostCodeineData <- `PostOp Codeine`
PostMethadoneData <- `PostOp Methadone` 
PostTramadolData <- `PostOp Tramadol`
PostOxycodoneData <- `PostOp Oxycodone`
PostBuprenorphineData <- `PostOp Buprenorphine`
PostMeperidineData <- `PostOp Meperidine`
PostPainScoreData <- `PostOp Admit Pain Score`
PostPainLevelData <- `PostOp Admit Pain Level`
PostPainScorePeakData <- `PostOp Peak Pain Score`
PostPainLevelPeakData <- `PostOp Peak Pain Level`
PACUDurationData <- `PACU Length of Stay`
PONVData <- `PONV - Y/N`
# New variables @July 21, 2019 Data set
ClassData <- `Patient Class`
PostAcetaminophenData <- `PostOp Acetaminophen IV - Y/N`
PostKetorolacData <- `PostOp Ketorolac IV - Y/N`
PCAData <- `PCA - Y/N`
#DetailedCPTData <- AnesCPT_DetailedCategory
# New variables @July 29, 2019 Data set
FentanylEndData <- `FENTANYL-END`
HydromorphoneEndData <- `HYDROMORPHONE-END`
MorphineEndData <- `MORPHINE-END`
#New variables @August 14, 2019
NewSurgData <- NewSurgType
NewProcData <- NewProcType
SleepData <- Sleepapnea_flag


# Functions
fx.nacount <- function(x, print = TRUE){
  for (i in 1:ncol(x)){
    fxnacount<-sum(is.na(x[,i]))
    cat(names(x)[i], "NA Total:", fxnacount, "  ")
    writeLines("\n")
  }
}

fx.mean <- function(x, print = TRUE){
  for (i in 1:ncol(x)){
    fxmean<-mean(na.exclude(x[,i]))
    cat(names(x)[i], "Mean:", fxmean, "  ")
    writeLines("\n")
  }
}

fx.median <- function(x, print = TRUE){
  for (i in 1:ncol(x)){
    fxmedian<-median(na.exclude(x[,i]))
    cat(names(x)[i], "Median:", fxmedian, " ")
    writeLines("\n")
  }
}

fx.quantile <- function(x, print = TRUE){
  for (i in 1:ncol(x)){
    fxquantile<-quantile(x[,i], c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99), na.rm = TRUE)
    cat(names(x)[i], "Quantiles: ", fxquantile, " ")
    writeLines("\n")
  }
}
  
fx.frequency <- function(x, print = TRUE){
  for (i in 1:ncol(x)){
    freqtable<-table(count(na.exclude(x[,i])))
    cat(names(x)[i], "Frequency Table:", freqtable, " ")
    writeLines("\n")
  }
}




### DATA ELEMENTS DERIVATION


## 1) Chronic Pain Data Element

# - - - - - - - - - -
ChronicPainData <- `Hx of Chronic Pain`
# History of Chronic Pain: WORDCLOUD
# ChronicDF <- na.exclude(data.frame(ChronicPainData))
# ChronicStr<-paste(ChronicDF[,1], collapse = "")
# chronic_corpus = Corpus(VectorSource(ChronicStr))
# chronic_corpus = tm_map(chronic_corpus, tolower)
# chronic_corpus = tm_map(chronic_corpus, removePunctuation)
# chronic_corpus = tm_map(chronic_corpus, stripWhitespace)
# dtmchronic <- DocumentTermMatrix(chronic_corpus)
# dtmchronic = as.matrix(dtmchronic)
# dtmchronic = t(dtmchronic)
# occurences = rowSums(dtmchronic)
# occurences = sort(occurences, decreasing=TRUE)
# wordcloud(head(names(occurences), 35), head(occurences, 35), scale = c(3,1))

ChronicPainData <- tolower(gsub(" ", "", ChronicPainData, fixed = TRUE))

# - - - - - - - - - - 
ChronicPain2Data <- `Medical Hx`
# Medical History: WORDCLOUD
# ChronicDF2 <- na.exclude(data.frame(ChronicPain2Data))
# ChronicStr2<-paste(ChronicDF2[,1], collapse = "")
# chronic_corpus2 = Corpus(VectorSource(ChronicStr2))
# chronic_corpus2 = tm_map(chronic_corpus2, tolower)
# chronic_corpus2 = tm_map(chronic_corpus2, removePunctuation)
# chronic_corpus2 = tm_map(chronic_corpus2, stripWhitespace)
# dtmchronic2 <- DocumentTermMatrix(chronic_corpus2)
# dtmchronic2 = as.matrix(dtmchronic2)
# dtmchronic2 = t(dtmchronic2)
# occurences2 = rowSums(dtmchronic2)
# occurences2 = sort(occurences2, decreasing=TRUE)
# wordcloud(head(names(occurences2), 35), head(occurences2, 35), scale = c(3,1))

ChronicPain2Data <- tolower(gsub(" ", "", ChronicPain2Data, fixed = TRUE))

# - - - - - - - - - - 
ChronicPain3Data <- `Neuro/Psych`
# Neuro/Psych: WORDCLOUD
# ChronicDF3 <- na.exclude(data.frame(ChronicPain3Data))
# ChronicStr3<-paste(ChronicDF3[,1], collapse = "")
# chronic_corpus3 = Corpus(VectorSource(ChronicStr2))
# chronic_corpus3 = tm_map(chronic_corpus3, tolower)
# chronic_corpus3 = tm_map(chronic_corpus3, removePunctuation)
# chronic_corpus3 = tm_map(chronic_corpus3, stripWhitespace)
# dtmchronic3 <- DocumentTermMatrix(chronic_corpus3)
# dtmchronic3 = as.matrix(dtmchronic3)
# dtmchronic3 = t(dtmchronic3)
# occurences3 = rowSums(dtmchronic3)
# occurences3 = sort(occurences3, decreasing=TRUE)
# wordcloud(head(names(occurences3), 35), head(occurences3, 35), scale = c(3,1))

ChronicPain3Data <- tolower(gsub(" ", "", ChronicPain3Data, fixed = TRUE))


## word indicators

# look for terms 'chronic' in the Hx of Chronic Pain Data
Chronic1 <- as.numeric(grepl("chronic", ChronicPainData)) 
# look for terms 'chronic pain' and 'chronic opioid' in the Medical Hx Data
Chronic2 <- as.numeric(grepl("chronicpain", ChronicPain2Data))
Chronic3 <- as.numeric(grepl("chronicopioid", ChronicPain2Data))
# look for terms 'chronic pain' and 'chronic opioid' in the Neuro/Psych Data
Chronic4 <- as.numeric(grepl("chronicpain", ChronicPain3Data))
Chronic5 <- as.numeric(grepl("chronicopioid", ChronicPain3Data))

ChronicCategoriesNonbinary <- Chronic1+Chronic2+Chronic3+Chronic4+Chronic5
ChronicCategoriesNonbinary[ChronicCategoriesNonbinary >1] <- 1
ChronicCategories <- ChronicCategoriesNonbinary
# (Grepl search feature: ChronicPainData[grepl("chronic", ChronicPainData) ==TRUE]  )



## 2) Medical Conditions Data Element

MedicalHXData <- ChronicPain2Data
MedicalHXTupper <- `Medical Hx`
MedicalHXTupper <- (gsub(" ", "", MedicalHXTupper, fixed = TRUE))
#Cardiac Disorder
Cardiacuppercase <- as.numeric(grepl("HTN|CAD|CABG", MedicalHXTupper))
Cardiaclowercase <- as.numeric(grepl("hypertension|coronarydisease|heartfailure|valvedisease|arrhythmia|cardiovasculardisease", MedicalHXData))
CardiacFData <- Cardiacuppercase + Cardiaclowercase
CardiacFData[CardiacFData > 1] <- 1

#Renal Dysfunction
RenalData <- as.numeric(grepl("renaldysfunction", MedicalHXData))

#Diabetes
DiabetesData <- as.numeric(grepl("diabetes", MedicalHXData))

#Cancer
CancerData <- as.numeric(grepl("cancer", MedicalHXData))

#Hepatic
HepaticData <- as.numeric(grepl("hepatic", MedicalHXData))

#Neurological
NeurologicalData <-as.numeric(grepl("neurologicaldisorder", MedicalHXData))

#Musculoskeletal
MusculoskeletalData <-as.numeric(grepl("musculoskeletaldisorder", MedicalHXData))

#PulmonaryData
PulmonaryData <- as.numeric(grepl("pulmonaryissue", MedicalHXData))

## 3) Smoking/Drug Use/Alcohol (MIHIR)
#  - - - - - - - - - - - - - - - - - - - - -


## 4) Mental Conditions Data Element

NeuroPsychData <- ChronicPain3Data

#Depression
DepressionData <- as.numeric(grepl("depression", NeuroPsychData))

#PTSD
PTSDData <- as.numeric(grepl("ptsd|posttraumaticstress", NeuroPsychData))

#Spinal Cord Injury
SCIData <- as.numeric(grepl("spinalcord", NeuroPsychData))

#Anxiety
AnxietyData <- as.numeric(grepl("anxiety", NeuroPsychData))



## 5) Current Meds Data Element

MedsData <- tolower(gsub(" ", "", `Current meds`, fixed = TRUE))
# morphine
MedsMorphineData <- as.numeric(grepl("morphine", MedsData))
# hydromorphone
MedsHydromorphoneData <- as.numeric(grepl("hydromorphone|dilaudid", MedsData))
# fentanyl
MedsFentanylData <- as.numeric(grepl("fentanyl", MedsData))
# hydromorphone
MedsHydromorphoneData <- as.numeric(grepl("hydromorphone|dilaudid", MedsData))

# all opioid meds
MedsData <-  tolower(`Current meds`)
MedsOpioidData <- as.numeric(grepl("morphine|ms contin|oramorph|sevredol|avinza|ordine|anamorph|kapanol|ms mono|doloral|statex|fentanyl|actiq|durogesic|abstral|onsolis|hydromorphone|dilaudid|palladone|zydone|methadone|methadose|dolophine|symoron|physeptone|metadol|oxycodone|roxicodone|oxycontin|oxecta|percocet|percodan|tylox|oxynorm|endone|targin|proladone|oxyneo|oxycocet|hydrocodone|vicodin|lorcet|lortab|jurnista|tussionex|vicoprofen|norco|tramadol|ryzolt|tramal|ultram|zydol|durela|tramacet|tridural|buprenorphine|suboxone|subutex|zubsolv|butrans|norspan|temgesic|codeine|co-codamol|tylenol 2|tylenol 3|tylenol 4|tapentadol|palexia|nucynta|pentazocine|talwin|trapentadol|nucynta|propoxyphene|darvon|wygesic", MedsData))



MedsNonOpioidData <- as.numeric(grepl("aspirin|acuprin|ecotrin|ibuprofen|advil|nurofen|motrin|brufen|paracetamol|acetaminophen|tylenol|panadol|naproxen|aleve|ketorolac|toradol|gabapentin|neurontin|gralise|horizant|pregabalin|lyrica|celecoxib|celebrex|diclofenac|voltaren|etodolac|lodine|prednisone|deltasone|prednicot|dexamethasone|decadron|methylprednisolone|medrol", MedsData))



## Inhalation Agents
Agents <- data.frame(SevofluraneData, IsofluraneData, DesfluraneData)
Agents[which(Agents$SevofluraneData < 5 & Agents$IsofluraneData < 5 & Agents$DesfluraneData < 5), ] <- NA
PrimaryAgent <- as.numeric(apply(Agents, 1, which.max))
PrimaryAgent[PrimaryAgent == 1] <- "Sevoflurane"
PrimaryAgent[PrimaryAgent == 2] <- "Isoflurane"
PrimaryAgent[PrimaryAgent == 3] <- "Desflurane"


## MME Calculation
PreopOpioid <- PreMorphineData + PreFentanylData + PreMethadoneData + PreMeperidineData + PreHydromophoneData + PreTramadolData + PreOxycodoneData
PreopOpioid[PreopOpioid > 0] <- 1
PreopOpioid[PreopOpioid == 0] <- 0

IntraopMME <- MorphineData*3 + FentanylData*0.3 + HydromorphoneData*20 + MethadoneData*3 + RemifentanilData*0.45 + AlfentanilData*0.075 + SufentanilData*3.0

PostopMME <- PostMorphineData*3 + PostFentanylData*0.3 + PostHydromorphoneData*20 + PostCodeineData*0.25 + PostMethadoneData*3 + PostBuprenorphineData*50 + PostMeperidineData*0.4 + PostOxycodoneData*1.5 + PostTramadolData*0.25 


## Binary Data Construction
KetamineData[KetamineData != 0] <- 1
KetamineData[KetamineData == 0] <- 0
KetorolacData[KetorolacData != 0] <- 1
KetorolacData[KetorolacData == 0] <- 0
AcetaminophenData[AcetaminophenData != 0] <- 1
AcetaminophenData[AcetaminophenData == 0] <- 0
EsmololData[EsmololData != 0] <- 1
EsmololData[EsmololData == 0] <- 0
LidocaineData[LidocaineData != 0] <- 1
LidocaineData[LidocaineData == 0] <- 0
DexmedetomidineData[DexmedetomidineData != 0] <- 1
DexmedetomidineData[DexmedetomidineData == 0] <- 0
NaloxoneData[NaloxoneData != 0] <- 1
NaloxoneData[NaloxoneData == 0] <- 0
PreAcetaminophenData[PreAcetaminophenData != 0] <- 1
PreAcetaminophenData[PreAcetaminophenData == 0] <- 0
PreGabapentinData[PreGabapentinData != 0] <- 1
PreGabapentinData[PreGabapentinData == 0] <- 0
PreCelecoxibData[PreCelecoxibData != 0] <- 1
PreCelecoxibData[PreCelecoxibData == 0] <- 0


## Fluids
FluidsInData <- CrystalloidsData + ColloidsData + BloodData
FluidsOutData <- UrineData + EBLData


## Pain Level Imputation
#Preop
# PrePainScoreData <- as.numeric(`Preop Pain Score`)
# PrePainScoreData[PrePainScoreData == 0] <- "None"
# PrePainScoreData[PrePainScoreData >= 1 & PrePainScoreData <= 3] <- "Mild"
# PrePainScoreData[PrePainScoreData >= 4 & PrePainScoreData <= 6] <- "Moderate"
# PrePainScoreData[PrePainScoreData == 7 | PrePainScoreData == 8 | PrePainScoreData == 9 | PrePainScoreData == 10] <- "Severe"
# PrePainScoreData <- ifelse(test = is.na(PrePainScoreData), yes = PrePainLevelData, no = PrePainScoreData)
# unique(PrePainScoreData)
PrePainScoreData <- as.character(`Preop Pain Score`)
PrePainScoreData[PrePainScoreData == "0"] <- "None"
PrePainScoreData[PrePainScoreData == "1"] <- "Mild"
PrePainScoreData[PrePainScoreData == "2"] <- "Mild"
PrePainScoreData[PrePainScoreData == "3"] <- "Mild"
PrePainScoreData[PrePainScoreData == "4"] <- "Moderate"
PrePainScoreData[PrePainScoreData == "5"] <- "Moderate"
PrePainScoreData[PrePainScoreData == "6"] <- "Moderate"
PrePainScoreData[PrePainScoreData == "7"] <- "Severe"
PrePainScoreData[PrePainScoreData == "8"] <- "Severe"
PrePainScoreData[PrePainScoreData == "9"] <- "Severe"
PrePainScoreData[PrePainScoreData == "10"] <- "Severe"
PrePainScoreData <- ifelse(test = is.na(PrePainScoreData), yes = PrePainLevelData, no = PrePainScoreData)
#Postop
PostPainScorePeakData <- as.character(`PostOp Peak Pain Score`)
PostPainScorePeakData[PostPainScorePeakData == "0"] <- "None"
PostPainScorePeakData[PostPainScorePeakData == "1" | PostPainScorePeakData == "2" | PostPainScorePeakData == "3"] <- "Mild"
PostPainScorePeakData[PostPainScorePeakData == "4" | PostPainScorePeakData == "5" | PostPainScorePeakData == "6"] <- "Moderate"
PostPainScorePeakData[PostPainScorePeakData == "7" | PostPainScorePeakData == "8" | PostPainScorePeakData == "9"| PostPainScorePeakData == "10"] <- "Severe"
PostPainScorePeakData <- ifelse(test = is.na(PostPainScorePeakData), yes = PostPainLevelPeakData, no = PostPainScorePeakData)


## PACU
PACUDurationData[PACUDurationData > 360 | PACUDurationData < 5] <- NA




### DATA OUTLIER CLEANUP

AgeData[AgeData == ">90"] <- 90 # Age must be capped at 90 years
AgeData <- as.numeric(AgeData)

RaceData <- str_replace(RaceData, "Declined to answer", "Declined to Answer")
RaceData <- str_replace(RaceData, "Declined to Answer", "Unavailable or Unknown")
RaceData <- str_replace(RaceData, "Native Hawaiian or other Pacific Islander", "Native Hawaiian or Other Pacific Islander")

WeightData <- as.numeric(WeightData)
WeightData[WeightData == 0 ] <- as.numeric(median(WeightData))
WeightData[WeightData > 500] <- 500 
WeightData[WeightData < 20] <- 25
WeightData[which(is.na(WeightData))] <- PostopOpioidDataSheet$`WEIGHT (lbs)`/2.205 # replace missing ORCA weight values


HeightData <- as.numeric(HeightData)
HeightData[HeightData == 0] <- as.numeric(median(HeightData))
HeightData[HeightData < 120] <- 120
HeightData[HeightData > 220] <- 220
HeightData[which(is.na(HeightData))] <- PostopOpioidDataSheet$`HEIGHT (in)`*2.54 # replace missing ORCA weight values


BMIData <- WeightData/(HeightData*0.01*HeightData*0.01)
BMIData[BMIData < 10] <- 10
BMIData[BMIData > 70] <- 70

SurgeryTypeData[SurgeryTypeData == "O-CARD" | SurgeryTypeData == "SURGCD"] <- NA
SurgeryAortic<-as.numeric(grepl("aortic", SurgeryDescData))
SurgeryAortic[SurgeryAortic == 1] <- NA

ProcDurationData[ProcDurationData == 0 & AnesDurationData >60] <- AnesDurationData[]-40
ProcDurationData[ProcDurationData > 1400 | ProcDurationData < 10] <- NA
ProcDurationData <- as.numeric(ProcDurationData)


SurgeryTypeData[SurgeryTypeData == "O-Gi" | SurgeryTypeData == "O-Rad"] <- "Other"
PositionData[PositionData == "Fowlers" | PositionData == "Jacknife" | PositionData == "LUD" | PositionData == "Modparkbench" | PositionData == "Parkbench" | PositionData == "RUD"] <- "Other"





### Creation of Final Data Frame
CompleteDataFrame <- data.frame(AgeData, BMIData, ProcDurationData, AnesDurationData, PropofolData, FluidsInData, FluidsOutData, IntraopMME,FentanylData, MorphineData, HydromorphoneData, RemifentanilData, KetamineData, KetorolacData, AcetaminophenData, FentanylEndData, HydromorphoneEndData, MorphineEndData, SleepData, PostopMME, SexData, RaceData, ASAData, NewSurgData, SurgeryAortic, GAData, TIVAData, LocalData, PositionData, PONVData, ChronicCategories, CardiacFData, RenalData, DiabetesData, CancerData, HepaticData, NeurologicalData, MusculoskeletalData, PulmonaryData, DepressionData, PTSDData, SCIData, AnxietyData, KetamineData, KetorolacData, AcetaminophenData, EsmololData, LidocaineData, DexmedetomidineData, NaloxoneData, PreopOpioid, MedsOpioidData, MedsNonOpioidData, PrePainScoreData, PostPainScorePeakData, ClassData, PostAcetaminophenData, PostKetorolacData, PCAData, PrimaryAgent,NewProcData)


#Data Purging
CompleteDataFrame <- CompleteDataFrame[!(is.na(CompleteDataFrame$BMIData)), ]
CompleteDataFrame <- CompleteDataFrame[!(is.na(CompleteDataFrame$ProcDurationData)), ]
CompleteDataFrame <- CompleteDataFrame[!(is.na(CompleteDataFrame$SurgeryAortic)), ]
CompleteDataFrame <- CompleteDataFrame[!(is.na(CompleteDataFrame$NewSurgData)), ]
CompleteDataFrame <- CompleteDataFrame[!(CompleteDataFrame$SexData == "U"), ]
CompleteDataFrame <- CompleteDataFrame[!(CompleteDataFrame$ClassData != "OP"), ]
CompleteDataFrame <- CompleteDataFrame[!(CompleteDataFrame$PostAcetaminophenData == "1"), ]
CompleteDataFrame <- CompleteDataFrame[!(CompleteDataFrame$PostKetorolacData == "1"), ]
CompleteDataFrame <- CompleteDataFrame[!(CompleteDataFrame$PCAData == "1"), ]


#Data Dependent Cleanup
CompleteDataFrame$FluidsInData[CompleteDataFrame$FluidsInData > as.numeric(quantile(CompleteDataFrame$FluidsInData, 0.99, na.rm = TRUE))] <- as.numeric(quantile(CompleteDataFrame$FluidsInData, 0.99, na.rm = TRUE))
CompleteDataFrame$FluidsOutData[CompleteDataFrame$FluidsOutData > as.numeric(quantile(CompleteDataFrame$FluidsOutData, 0.99, na.rm = TRUE))] <- as.numeric(quantile(CompleteDataFrame$FluidsOutData, 0.99, na.rm = TRUE))
CompleteDataFrame$IntraopMME[CompleteDataFrame$IntraopMME > as.numeric(quantile(CompleteDataFrame$IntraopMME, 0.99, na.rm = TRUE))] <- as.numeric(quantile(CompleteDataFrame$IntraopMME, 0.99, na.rm = TRUE))
CompleteDataFrame$PostopMME[CompleteDataFrame$PostopMME > as.numeric(quantile(CompleteDataFrame$PostopMME, 0.99, na.rm = TRUE))] <- as.numeric(quantile(CompleteDataFrame$PostopMME, 0.99, na.rm = TRUE))

#PostopMME bins conversion 
bins <- c(-Inf, 3, 11, 25, Inf)
binsnames <- c("None", "Low", "Medium", "High")
CompleteDataFrame$PostopMMENum <- CompleteDataFrame$PostopMME
CompleteDataFrame$PostopMME <- cut(CompleteDataFrame$PostopMME, breaks = bins, labels = binsnames)

#What variables will be used in the final model dataset?
keeps <- c("AgeData", "BMIData", "ProcDurationData", "PostopMME","SleepData", "PostopMMENum", "SexData", "RaceData", "ASAData", "NewSurgData", "ChronicCategories", "CardiacFData", "RenalData", "DiabetesData", "CancerData", "HepaticData", "NeurologicalData", "MusculoskeletalData", "PulmonaryData", "DepressionData", "PTSDData", "SCIData", "AnxietyData", "PreopOpioid", "MedsOpioidData", "MedsNonOpioidData", "PrePainScoreData", "NewProcData")

CompleteDataFrame <- CompleteDataFrame[keeps]

#"PropofolData" "FluidsInData", "FluidsOutData", "IntraopMME", "FentanylData", "MorphineData", "HydromorphoneData", "RemifentanilData", "FentanylEndData", "HydromorphoneEndData", "MorphineEndData", "PositionData","GAData", "TIVAData", "LocalData", "KetamineData", "KetorolacData", "AcetaminophenData", "EsmololData", "LidocaineData", "DexmedetomidineData", "NaloxoneData","PrimaryAgent"


### BASIC MODELLING MULTINOMIAL
NumericalDataFrame <- CompleteDataFrame[,c(1:match("PostopMMENum", names(CompleteDataFrame)))]
CategoricalDataFrame <- CompleteDataFrame[,-c(1:match("PostopMMENum", names(CompleteDataFrame)))]
CategoricalDataFrame[c(1:ncol(CategoricalDataFrame))] <- lapply(CategoricalDataFrame[c(1:ncol(CategoricalDataFrame))], factor)  ## as.factor() could also be used
dummyDF <- dummy_cols(CategoricalDataFrame, remove_first_dummy = TRUE)
dummyDF <- dummyDF[,-c(1:as.numeric(ncol(CategoricalDataFrame)))]
dummyDF <- data.frame(dummyDF)
CompleteDF <- cbind(NumericalDataFrame,dummyDF)
#CompleteDF <- CompleteDF[complete.cases(CompleteDF),]
CompleteDF$PostopMME <- factor(CompleteDF$PostopMME)
CompleteDF$PostopMME <- relevel(CompleteDF$PostopMME, ref = "None")
#CompleteDF$PostopMME <- NULL # remove this line to do categorical models
split = 0.8
dataIndex <- createDataPartition(CompleteDF$PostopMME, p=split, list=FALSE)
data_train <- CompleteDF[dataIndex,]
data_test <- CompleteDF[-dataIndex,]
# 
# 
# ### Numerical MODELS
# 
# ## Neural Network Model
# 
# ridgemodel <- linearRidge(PostopMMENum ~ AgeData+BMIData+ProcDurationData+PropofolData+FluidsInData+FluidsOutData+IntraopMME+ASAData_4+ASAData_2+ASAData_1, data = data_train)
# predictionsridge <- predict(ridgemodel, data_test)
# cat("Test Accuracy Ridge: ", (1-MAPE(data_test$PostopMMENum, predictionsridge)), "\n")
# 
# # svmmodel <- svm(PostopMMENum ~ BMIData + ProcDurationData + PropofolData + FluidsInData + FluidsOutData + IntraopMME + FentanylEndData + HydromorphoneEndData + SurgeryTypeData_PLASTIC + SurgeryTypeData_ORTHO + SurgeryTypeData_O.GI + GAData_LMA + ChronicCategories_1 + AnxietyData + PrePainScoreData_Moderate+PrePainScoreData_Severe, data = data_train)
# # predictionssvm <- predict(svmmodel, data_test)
# # cat("Test Accuracy SVM: ", (1-MAPE(data_test$PostopMMENum, predictionssvm)), "\n")
# 
# opioidmodelxg <- xgboost(data = data.matrix(data_train[-11]), label = data_train$PostopMMENum, nrounds = 50, max_depth = 5, eta= 0.2)
# predictionsxg <- predict(opioidmodelxg, data.matrix(data_test[-11]))
# cat("Test Accuracy XG num: ", (1-MAPE(data_test$PostopMMENum, predictionsxg)))
# View(data.frame(data_test$PostopMMENum, predictionsxg))
# 
# 
### Categorical MODELS

## Multinomial Model
data_train$PostopMMENum <- NULL
data_test$PostopMMENum <- NULL #We are not using numeric, so I removed the numeric PostopMME 
mnmodel <- multinom(PostopMME~., data = data_train)
predictions<-round(predict(mnmodel,data_test, type = "prob"), 3)
resultstable<-data.frame(predictions, data_test$PostopMME)
resultstable$nonelow <- resultstable$None + resultstable$Low
resultstable$lowmedium <- resultstable$Medium + resultstable$Low
resultstable$mediumhigh <- resultstable$Medium + resultstable$High
resultstable$prediction <- colnames(resultstable)[apply(resultstable,1,which.max)]

resultstable$result[resultstable$prediction == "nonelow" & (resultstable$data_test.PostopMME == "None" | resultstable$data_test.PostopMME == "Low")] <- "Correct"
resultstable$result[resultstable$prediction == "nonelow" & (resultstable$data_test.PostopMME != "None" & resultstable$data_test.PostopMME != "Low")] <- "Incorrect"

resultstable$result[resultstable$prediction == "lowmedium" & (resultstable$data_test.PostopMME == "Low" | resultstable$data_test.PostopMME == "Medium")] <- "Correct"
resultstable$result[resultstable$prediction == "lowmedium" & (resultstable$data_test.PostopMME != "Low" & resultstable$data_test.PostopMME != "Medium")] <- "Incorrect"

resultstable$result[resultstable$prediction == "mediumhigh" & (resultstable$data_test.PostopMME == "Medium" | resultstable$data_test.PostopMME == "High")] <- "Correct"
resultstable$result[resultstable$prediction == "mediumhigh" & (resultstable$data_test.PostopMME != "Medium" & resultstable$data_test.PostopMME != "High")] <- "Incorrect"

print(length(resultstable$result[resultstable$result=="Correct"])/nrow(resultstable))

# 
# ## XG Boost Model
# 
# # Factor to Integer class
# CompleteDF <- cbind(NumericalDataFrame,dummyDF)
# postopmme <- as.factor(CompleteDF$PostopMME)
# label <- as.integer(CompleteDF$PostopMME)-1
# CompleteDF$PostopMME <- NULL
# CompleteDF$PostopMMENum <- NULL
# # Data Split
# n = nrow(CompleteDF)
# train.index = sample(n,floor(0.8*n))
# test.index = sample(n,floor(0.2*n))
# train.data = as.matrix(CompleteDF[train.index,])
# train.label = label[train.index]
# test.data = as.matrix(CompleteDF[test.index,])
# test.label = label[test.index]
# 
# 
# #Matrix
# xgb.train <- xgb.DMatrix(data=train.data, label = train.label)
# xgb.test <- xgb.DMatrix(data=test.data, label = test.label)
# 
# # Define parameters for classification
# num_class = length(levels(postopmme))
# params = list(
#   booster="gbtree",
#   eta=0.001,
#   max_depth=5,
#   gamma=3,
#   subsample=0.75,
#   colsample_bytree=1,
#   objective="multi:softprob",
#   eval_metric="mlogloss",
#   num_class=num_class
# )
# 
# # Fit the model
# xgb.fit=xgb.train(
#   params=params,
#   data=xgb.train,
#   nrounds=100,
#   nthreads=1,
#   early_stopping_rounds=10,
#   watchlist=list(val1=xgb.train,val2=xgb.test),
#   verbose=0
# )
# 
# # Review the final model and results
# 
# 
# xgb.pred = predict(xgb.fit,test.data,reshape=T)
# xgb.pred = as.data.frame(xgb.pred)
# colnames(xgb.pred) = levels(postopmme)
# xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
# xgb.pred$label = levels(postopmme)[test.label+1]
# result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
# print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))
# # source: https://rstudio-pubs-static.s3.amazonaws.com/456044_9c275b0718a64e6286751bb7c60ae42a.html
# # bins <- c(-Inf, 3, 15, 25, Inf)
# # binsnames <- c("None", "Low", "Middle", "High")
# # categorypredictionsxg <- cut(predictionsxg, breaks=bins, labels = binsnames)
# # categorytestxg <- cut(data_test$PostopMME, breaks=bins, labels = binsnames)
# # View(data.frame(categorypredictionsxg, categorytestxg))
# cat("Test Error XG: ", (MAPE(data_test$PostopMME, predictionsxg)))
#cat("Categorical Score", (sum(categorypredictionsxg==categorytestxg))/length(categorypredictionsxg))
# 
# 
# linear <- lm(PostopMME ~ AgeData, data=data_train)
# predictionslm <- predict(linear, data_test[-9])
# cat("Test Accuracy Linear: ", (1-MAPE(data_test$PostopMME, predictionslm)))
# # fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
# # fit <- train(data_train$PostopMME ~ ., data = data_train, method = "gbm", trControl = fitControl,verbose = FALSE)
# # predicted= predict(fit,data_test[-9],type= "prob")[,2] 
# # opioidmodelrf <- randomForest(PostopMME ~ ., data=data_train, max_features = 10)
# # predictionsrf <- predict(opioidmodelrf, data_test[-1])
# # cat("Test Accuracy RF: ", (1-MAPE(data_test$PostopMME, predictionsrf)))















### GRAPH CODING

# AgeHist<-ggplot(PostopOpioidDataSheet, aes(AgeData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Age (yrs)") + ylab("Freq")
# WeightHist<-ggplot(PostopOpioidDataSheet, aes(WeightData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Weight (lbs)") + ylab("Freq")
# HeightHist<-ggplot(PostopOpioidDataSheet, aes(HeightData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Height (in)") + ylab("Freq")
# BMIHist<-ggplot(PostopOpioidDataSheet, aes(BMIData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("BMI") + ylab("Freq")
# ProcDurationHist<-ggplot(PostopOpioidDataSheet, aes(ProcDurationData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Procedure Duration (min)") + ylab("Freq")
# AnesDurationHist<-ggplot(PostopOpioidDataSheet, aes(AnesDurationData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Anesthesia Duration (min)") + ylab("Freq")
# PropofolHist<-ggplot(PostopOpioidDataSheet, aes(PropofolData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Propofol(mg)") + ylab("Freq")
# FluidsInHist<-ggplot(PostopOpioidDataSheet, aes(FluidsInData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Fluids IN(ml)") + ylab("Freq")
# FluidsOutHist<-ggplot(PostopOpioidDataSheet, aes(FluidsOutData)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Fluids OUT(ml)") + ylab("Freq")
# IntraopMMEHist<-ggplot(PostopOpioidDataSheet, aes(IntraopMME)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Intraoperative MME") + ylab("Freq") + xlim(c(0,400))
# PostopMMEHist<-ggplot(PostopOpioidDataSheet, aes(PostopMME)) + geom_histogram(bins = 20, col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Postoperative MME") + ylab("Freq") + xlim(c(0,250))
# 
# 
# 
# SexBar<-ggplot(PostopOpioidDataSheet, aes(SexData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Sex") + ylab("Freq")
# RaceBar<-ggplot(PostopOpioidDataSheet, aes(RaceData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Race") + ylab("Freq")
# ASABar<-ggplot(PostopOpioidDataSheet, aes(ASAData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("ASA") + ylab("Freq")
# SurgeryTypeBar<-ggplot(PostopOpioidDataSheet, aes(SurgeryTypeData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Surgery Type") + ylab("Freq")
# GATypeBar<-ggplot(PostopOpioidDataSheet, aes(GAData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("GA Type") + ylab("Freq")
# TIVABar<-ggplot(PostopOpioidDataSheet, aes(TIVAData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("TIVA") + ylab("Freq")
# LocalBar<-ggplot(PostopOpioidDataSheet, aes(LocalData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Local") + ylab("Freq")
# PositionBar<-ggplot(PostopOpioidDataSheet, aes(PositionData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Position") + ylab("Freq")
# PONVBar<-ggplot(PostopOpioidDataSheet, aes(PONVData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("PONV") + ylab("Freq")
# ChronicBar<-ggplot(PostopOpioidDataSheet, aes(ChronicCategories)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Chronic Pain") + ylab("Freq")
# CardiacBar<-ggplot(PostopOpioidDataSheet, aes(CardiacFData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Cardiac Condition") + ylab("Freq")
# RenalBar<-ggplot(PostopOpioidDataSheet, aes(RenalData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Renal Condition") + ylab("Freq")
# DiabetesBar<-ggplot(PostopOpioidDataSheet, aes(DiabetesData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Diabetes Condition") + ylab("Freq")
# CancerBar<-ggplot(PostopOpioidDataSheet, aes(CancerData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Cancer Condition") + ylab("Freq")
# HepaticBar<-ggplot(PostopOpioidDataSheet, aes(RenalData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Hepatic Condition") + ylab("Freq")
# NeurologicalBar<-ggplot(PostopOpioidDataSheet, aes(RenalData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Neurological Condition") + ylab("Freq")
# MusculoskeletalBar<-ggplot(PostopOpioidDataSheet, aes(RenalData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Musculoskeletal Condition") + ylab("Freq")
# DepressionBar<-ggplot(PostopOpioidDataSheet, aes(DepressionData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Depression") + ylab("Freq")
# PTSDBar<-ggplot(PostopOpioidDataSheet, aes(PTSDData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("PTSD") + ylab("Freq")
# SCIBar<-ggplot(PostopOpioidDataSheet, aes(SCIData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Spinal Cord Injry") + ylab("Freq")
# KetamineBar<-ggplot(PostopOpioidDataSheet, aes(KetamineData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Ketamine(mg)") + ylab("Freq")
# KetorolacBar<-ggplot(PostopOpioidDataSheet, aes(KetorolacData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Ketorolac(mg)") + ylab("Freq")
# AcetaminophenBar<-ggplot(PostopOpioidDataSheet, aes(AcetaminophenData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Acetaminophen(mg)") + ylab("Freq")
# EsmololBar<-ggplot(PostopOpioidDataSheet, aes(EsmololData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Esmolol(mg)") + ylab("Freq")
# LidocaineBar<-ggplot(PostopOpioidDataSheet, aes(LidocaineData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Lidocaine(mg)") + ylab("Freq")
# DexmedetomidineBar<-ggplot(PostopOpioidDataSheet, aes(DexmedetomidineData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Dexmedetomidine(mg)") + ylab("Freq")
# NaloxoneBar<-ggplot(PostopOpioidDataSheet, aes(NaloxoneData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Naloxone(mg)") + ylab("Freq")
# PreopOpioidBar<-ggplot(PostopOpioidDataSheet, aes(PreopOpioid)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Preop Opioid") + ylab("Freq")
# OpioidHomeBar<-ggplot(PostopOpioidDataSheet, aes(MedsOpioidData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Opioid Medication") + ylab("Freq")
# NonOpioidHomeBar<-ggplot(PostopOpioidDataSheet, aes(MedsNonOpioidData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Non-Opioid Medication") + ylab("Freq")
# PreopPainBar<-ggplot(PostopOpioidDataSheet, aes(PrePainScoreData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Pain Score Peak (Postop)") + ylab("Freq")
# PostopPeakPainBar<-ggplot(PostopOpioidDataSheet, aes(PostPainScorePeakData)) + geom_bar(col = "grey60", fill = "grey10", alpha = 0.7) + xlab("Pain Score Peak (Postop)") + ylab("Freq")


# corrplot(cor(dummyDF, use = "na.or.complete"), method="circle", tl.pos = 'n')
# corrplot(cor(NumericalDataFrame, use = "na.or.complete"), method="number")


# corrplot(cor(CompleteDF, use = "na.or.complete"), method="circle", tl.pos = 'n')
 
# CompleteDF <- CompleteDF[complete.cases(CompleteDF),]
# CompleteDF> CompleteDF[, -c(56:59)]
# split = 0.8
# dataIndex <- createDataPartition(CompleteDF$PostopMME, p=split, list=FALSE)
# data_train <- CompleteDF[dataIndex,]
# data_test <- CompleteDF[-dataIndex,]
#
# opioidmodelxg <- xgboost(data = data.matrix(data_train[-1]), label = data_train$PostopMME, nrounds = 50, max_depth = 5, eta= 0.2)
# predictionsxg <- predict(opioidmodelxg, data.matrix(data_test[-1]))
# cat("Test Accuracy XG: ", (1-MAPE(data_test$PostopMME, predictionsxg)))

# opioidmodelrf <- randomForest(PostopMME ~ ., data=data_train, max_features = 10)
# predictionsrf <- predict(opioidmodelrf, data_test[-1])
# cat("Test Accuracy RF: ", (1-MAPE(data_test$PostopMME, predictionsrf)))

