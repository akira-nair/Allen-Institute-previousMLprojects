## APP
library(caret)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(stringr)
library(xgboost)
library(randomForest)

# IMPORT DATA
DataBook<-read.csv("RestrictedDataBook.csv")

EmrgDataBook<-read.csv("EgData.csv")

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


#Categories 
dtcategories <- cut(DMData, 3, labels = c("UD", "1", "2"))
emrgcategories <- cut(EMRGData, 2, labels = c("No EMRG", "EMRG"))
anescategories <- cut(AnesData, 5, labels = c("Other", "Intracranial", "Cardiac", "Liver X", "Spinal"))



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
hbData <- as.numeric(hbData)
#HyperGData <- as.factor(HyperGData)
alldatana = data.frame(PeakGData, AgeData, BMIData, PreGData, hbData, Duration, 
                       sexm.dummy, asa2.dummy, asa3.dummy, asa4.dummy, asa5.dummy, iyes.dummy, 
                       dt1.dummy, dt2.dummy, ohayes.dummy, classin.dummy, emrgyes.dummy, 
                       cardio.dummy, general.dummy, gynecology.dummy, neurology.dummy,
                       oral.dummy, orthopedic.dummy, otolaryncology.dummy, plastic.dummy,
                       thoracic.dummy, transplant.dummy, urology.dummy, vascular.dummy,
                       intracranial.dummy, cardiac.dummy, liverx.dummy, spinal.dummy)

alldata <- alldatana[complete.cases(alldatana),]
#alldata <- data.matrix(alldata)
split=0.8
dataIndex <- createDataPartition(alldata$PeakGData, p=split, list=FALSE)
data_train <- alldata[dataIndex,]
data_test <- alldata[-dataIndex,]
appPeakG.xg = xgboost(data = data.matrix(alldata[-1]), label = alldata$PeakGData, nrounds = 50, max_depth = 7, eta = 0.1, gamma = 10)
#appPeakG.xg = randomForest(PeakGData ~ ., data=data_train)


# app.PeakG.rf <- randomForest(PeakGData ~ AgeData + BMIData + PreGData + hbData + Duration +
#                 sexm.dummy + asa2.dummy + asa3.dummy + asa4.dummy + asa5.dummy + iyes.dummy +
#                 dt1.dummy + dt2.dummy + ohayes.dummy + classin.dummy + emrgyes.dummy+
#                 cardio.dummy + general.dummy + gynecology.dummy + neurology.dummy +
#                 oral.dummy + orthopedic.dummy + otolaryncology.dummy + plastic.dummy +
#                 thoracic.dummy + transplant.dummy + urology.dummy + vascular.dummy +
#                 intracranial.dummy + cardiac.dummy + liverx.dummy + spinal.dummy, data = alldata)


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



ui <- dashboardPage(
  dashboardHeader(title = "HYPER-G"),
  
  dashboardSidebar(sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("calculator")),
                   menuItem("Project Information", tabName = "info", icon = icon("leaf")),
                   menuItem("Instructions", tabName = "tutorial", icon = icon("info")))),
  
  dashboardBody(
    tags$head(tags$style(HTML('.skin-blue .main-header .logo {
                              background-color: #5050c9;}
                              .skin-blue .main-header .navbar {
                              background-color: #4c4cc9;}
                              .skin-blue .main-header .logo:hover {
                              background-color: #8484ff;}
                              .skin-blue .main-header .navbar .sidebar-toggle:hover {
                              background-color: #8484ff;}
                              .skin-blue .main-sidebar {
                              background-color: #20203a;}
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #005b99;}
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #1d3342;
                              color: #ffffff;
                              }
                              }

  
                              '
                              ))),
                              
    tabItems(
      #Main page
      tabItem(tabName = "dashboard", 
              splitLayout(
              #left side  
              wellPanel(numericInput(inputId = "ageinput", label = "Age", value = 50, min = 18, max = 99, step = 1),
              radioButtons(inputId = "sexinput", label = "Sex", c("Male", "Female")),
              numericInput(inputId = "weightinput", label = "Weight (kg)", value = 75, min = 30, max = 500),
              numericInput(inputId = "heightinput", label = "Height (cm)", value = 175, min = 125, max = 240),
              numericInput(inputId = "hbinput", label = "Hemoglobin A1C", value = 5, min = 4, max = 14, step = 0.1),
              numericInput(inputId = "durationinput", label = "Projected Duration (min)", value = 120, min = 40, max = 960),
              radioButtons(inputId = "asainput", label = "American Society of Anesthesiology(ASA) Score", c("1", "2", "3", "4", "5")),
              radioButtons(inputId = "insulininput", label = "Preop Insulin", c("No", "Yes")),
              radioButtons(inputId = "ohainput", label = "Preop Oral Hyperglycemic Agent", c("No", "Yes")),
              radioButtons(inputId = "classinput", label = "Patient Class", c("Inpatient", "Outpatient"))
              
              ),
              
              #right side
              wellPanel(numericInput(inputId = "preopinput", label = "Last Preoperative Glucose Value", value = 110, min = 60, max = 400),
              radioButtons(inputId = "emrginput", label = "Emergency Status", c("Not Emergency Procedure", "Emergency Procedure")),
              radioButtons(inputId = "dtinput", label = "Diabetic Type", c("Undefined", "1", "2")),
              radioButtons(inputId = "surgeryinput", label = "Surgery Type", c("Anesthesia Off-site", "Cardiac Surgery", "General Surgery", "Gynecology", "Neurosurgery", "Oral Surgery", 
                                                                                "Orthopedic", "Otolaryncology", "Plastic Surgery", "Thoracic Surgery", "Transplant", "Urology",
                                                                                "Vascular Surgery")),
              radioButtons(inputId = "anesinput", label = "Surgery Type Modifiers", c("None","Intracranial","Cardiopulmonary Bypass","Liver Transplant","Spinal")),
              actionButton("go", label = "predict Peak Glucose"),
              textOutput("Predicted peak glucose level during surgery"),
              tags$head(tags$style("#Prediction{
                                 color: red;
                                 font-size: 18px;
                                 font-style: bold;
                                   }"
                         ))
              
              ))
              ),
              
      
              
      #Project Info Page
      tabItem(tabName = "info", h1("Project Information"), p("During surgical procedures, a patient’s glucose levels tend to rise due to the influx of stress hormones. These 
stress hormones can lead to hyperglycemia, where their glucose levels are significantly
higher than the healthy levels. Uncontrolled hyperglycemia in surgery patients has 
been associated with poor postoperative outcome in both diabetic and non-diabetic 
patients. Hence, close management of glucose levels is typically undertaken by the 
perioperative team.  It is important to initiate glucose management in a timely manner
to avoid excessive and prolonged hyperglycemia. Towards this it would be helpful to 
know in advance which patients will develop hyperglycemia during the surgery. 
Further, expensive and limited perioperative resources such as glucose meters 
and insulin infusion preparations, can be proactively prepared and optimally utilized.
This web-based tool is designed to help the perioperative care team estimate 
hyperglycemia in a patient prior to surgery. The tool uses a machine learning model
to predict hyperglycemia using a set of preoperative data entered by the user.   
                                                             ")),
      #Tutorial Page
      tabItem(tabName = "tutorial", h1("Instructions"), p("To use this application, proceed to the home page
                                                          and enter the relevant preoperative values for each 
                                                          variable. Once the form has been completed, click the
                                                          button that says 'Predict Hyperglycemia' and the model
                                                          result will be displayed. The displayed value is the peak
                                                          glucose level estimated during surgery. If data for a 
                                                          variable is unknown the program imputes the value. "))
    )
    
    

  )
  )

#### SERVER
server <- function(input,output) {
  observeEvent(input$go, output$Prediction <- renderPrint({
    .v1<-input$ageinput
    .v2<-input$weightinput/((input$heightinput*0.01)^2)
    .v3<-input$preopinput
    .v4<-input$hbinput
    .v5<-input$durationinput
    .v6<-as.numeric(input$sexinput == "Male")
    .v7<-as.numeric(input$asainput == "2")
    .v8<-as.numeric(input$asainput == "3")
    .v9<-as.numeric(input$asainput == "4")
    .v10<-as.numeric(input$asainput == "5")
    .v11<-as.numeric(input$insulininput == "Yes")
    .v12<-as.numeric(input$dtinput == "1")
    .v13<-as.numeric(input$dtinput == "2")
    .v14<-as.numeric(input$ohainput == "Yes")
    .v15<-as.numeric(input$classinput == "Inpatient")
    .v16<-as.numeric(input$emrginput == "Emergency Procedure")
    .v17<-as.numeric(input$surgeryinput == "Cardiac Surgery")
    .v18<-as.numeric(input$surgeryinput == "General Surgery")
    .v19<-as.numeric(input$surgeryinput == "Gynecology")
    .v20<-as.numeric(input$surgeryinput == "Neurosurgery")
    .v21<-as.numeric(input$surgeryinput == "Oral Surgery")
    .v22<-as.numeric(input$surgeryinput == "Orthopedic")
    .v23<-as.numeric(input$surgeryinput == "Otolaryncology")
    .v24<-as.numeric(input$surgeryinput == "Plastic Surgery")
    .v25<-as.numeric(input$surgeryinput == "Thoracic Surgery")
    .v26<-as.numeric(input$surgeryinput == "Transplant")
    .v27<-as.numeric(input$surgeryinput == "Urology")
    .v28<-as.numeric(input$surgeryinput == "Vascular Surgery")
    .v29<-as.numeric(input$anesinput == "Intracranial")
    .v30<-as.numeric(input$anesinput == "Cardiopulmonary Bypass")
    .v31<-as.numeric(input$anesinput == "Liver Transplant")
    .v32<-as.numeric(input$anesinput == "Spinal")
    
    writeLines("Predicted Glucose Value:")
    predictedvalue <- GlucosePredictingFunction(.v1, .v2, .v3, .v4, .v5, .v6, .v7, .v8, .v9, .v10, .v11, .v12, .v13, .v14, .v15, 
                                                .v16, .v17, .v18, .v19, .v20, .v21, .v22, .v23, .v24, .v25, .v26, .v27, .v28, .v29, .v30, .v31, .v32)
    writeLines(predictedvalue)
    writeLines("mg/dl")
    
    
    
    
  }))
  

}




#run
shinyApp(ui = ui, server = server)




