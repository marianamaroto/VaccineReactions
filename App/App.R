library(shiny)
library(shinydashboard)
library(ggplot2)
library(FNN)
library(RColorBrewer)

ui <- dashboardPage(
  dashboardHeader(title="Adverse Reaction Classifcation and Clusters of the COVID-19 Vaccine: Potential Clinical Prediction Tool"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    h3("Adverse Reaction Classification and Clusters of the COVID-19 Vaccine: Potential Clinical Prediction Tool"),
    h4("Andrea Gomez, Dung Mai, Mariana Maroto"),
    h4("Graduate Center, CUNY - Machine Learning CSCI 740 Spring 2021"),
    h5("Our application project clusters and classifies COVID-19 vaccine adverse reactions. The purpose of the project is having a detailed understanding of the common types of adverse reactions and  identify which adverse reactions are in need of immediate care."),
    h5("The dataset is provided by the Vaccine Adverse Event Reporting System VAERS and contains reports about adverse events that may be associated with COVID-19 vaccines."),
    fluidRow(tabBox(
      title = 'COVID-19 Vaccine Reactions',
      id = 'tabset1', width = 12,
      tabPanel("Classification",
        fluidRow(
          column(width = 4,
            h4("Please Enter Patient Information:"),
            selectInput("Gender", "Gender:", list('Male','Female')),
            numericInput("AGE_YRS","Age:",value = 50, min = 0, max = 110),
            checkboxGroupInput("medicalhist","Medical History:", choices = c("Allergies","Asthma",
                                                                             "Hypertension","Diabetes",
                                                                             "Hypothyroid")),
            checkboxGroupInput("allergies","Specific Allergies:", choices = c("Penicillin","Sulfa")),
            selectInput("Vaccine", "Vaccine Manufacturer:", list('Pfizer/Biontech','Moderna','Janssen')),
            numericInput("NUMDAYS","Days Since Vaccine Shot:",value = 2, min = 0, max = 100),
          ),
          column(width = 4,
            checkboxGroupInput("symptoms","Adverse Vaccine Reactions:", choices = c("Arthralgia",
                                                                                    "Asthenia",
                                                                                    "COVID-19",
                                                                                    "Chills",
                                                                                    "Cough",
                                                                                    "Diarrhoea",
                                                                                    "Dizziness",
                                                                                    "Dyspnoea",
                                                                                    "Erythema",
                                                                                    "Fatigue",
                                                                                    "Feeling abnormal",
                                                                                    "Headache",
                                                                                    "Hyperhidrosis",
                                                                                    "Hypoaesthesia",
                                                                                    "Injection site erythema",
                                                                                    "Injection site pain",
                                                                                    "Injection site pruritus",
                                                                                    "Injection site swelling",
                                                                                    "Injection site warmth",
                                                                                    "Lymphadenopathy",
                                                                                    "Malaise",
                                                                                    "Myalgia",
                                                                                    "Nausea",
                                                                                    "Pain",
                                                                                    "Pain in extremity",
                                                                                    "Paraesthesia",
                                                                                    "Pruritus",
                                                                                    "Pyrexia",
                                                                                    "Rash",
                                                                                    "SARS-CoV-2 test negative",
                                                                                    "SARS-CoV-2 test positive",
                                                                                    "Urticaria",
                                                                                    "Vomiting"))),
          column(width = 4,
            submitButton("Apply Changes"),
            h4("Probability of having a life threat reaction:"),
            h3(textOutput("threatscore")),
            h5("Disclaimer: Accuracy is around 90%.")
      ))),
      tabPanel("Clustering",
               h3("Clusters:"),
               h5("Cluster 1: 9% of adverse reaction cases. Symptoms involve mostly injection site pain, swelling, redness. Life threat probability is low at 1%. Skews towards females and more likely with Moderna vaccine."),
               h5("Cluster 2: 11% of adverse reaction cases. Main symptoms are dizziness and nausea. Life threat probability is 13%. More likely to be weeks after vaccination."),
               h5("Cluster 3: 19% of adverse reaction cases. Main symptoms are chills, pyrexia/fever, and pain. Life threat probability is 6%. Days shortly after vaccination."),
               h5("Cluster 4: 16% of adverse reaction cases. Main symptoms are headache and fatigue. Life threat probability is 8%."),
               h5("Cluster 5: 45% of adverse reaction cases. Has a mix of all main symptoms. Life threat probability is highest at 19%."),
               h4(textOutput("predictedcluster")),
               fluidRow(
                 column(plotOutput("plotcluster1"),width = 6),
                 column(plotOutput("plotcluster2"),width = 6)),
               fluidRow(
                 column(plotOutput("plotcluster3"),width = 6),
                 column(plotOutput("plotcluster4"),width = 6)))
    ))
))

library(randomForest)
server <- function(input,output) {

  output$threatscore <- renderText({

    machinelearning <- function(Gender, AGE_YRS, Vaccine, NUMDAYS, symptoms, allergies, medicalhist) {
      test_data = read.csv("test_data_headers.csv", header = TRUE)
      load('ClassificationModel.rda') #load saved model
      
      if (Gender == 'Female') {test_data$Gender = 1} else {test_data$Gender = 0}
      test_data$AGE_YRS = AGE_YRS
      test_data$NUMDAYS = NUMDAYS
      
      if (Vaccine == 'Moderna') {test_data$MODERNA = 1}
      if (Vaccine == 'Pfizer/Biontech') {test_data$PFIZER.BIONTECH = 1}
      
      if ("Arthralgia" %in% symptoms) {test_data$Arthralgia = 1}
      if ("Asthenia" %in% symptoms) {test_data$Asthenia = 1}
      if ("COVID-19" %in% symptoms) {test_data$COVID.19 = 1}
      if ("Chills" %in% symptoms) {test_data$Chills = 1}
      if ("Cough" %in% symptoms) {test_data$Cough = 1}
      if ("Diarrhoea" %in% symptoms) {test_data$Diarrhoea = 1}
      if ("Dizziness" %in% symptoms) {test_data$Dizziness = 1}
      if ("Dyspnoea" %in% symptoms) {test_data$Dyspnoea = 1}
      if ("Erythema" %in% symptoms) {test_data$Erythema = 1}
      if ("Fatigue" %in% symptoms) {test_data$Fatigue = 1}
      if ("Feeling abnormal" %in% symptoms) {test_data$Feeling.abnormal = 1}
      if ("Headache" %in% symptoms) {test_data$Headache = 1}
      if ("Hyperhidrosis" %in% symptoms) {test_data$Hyperhidrosis = 1}
      if ("Hypoaesthesia" %in% symptoms) {test_data$Hypoaesthesia = 1}
      if ("Injection site erythema" %in% symptoms) {test_data$Injection.site.erythema = 1}
      if ("Injection site pain" %in% symptoms) {test_data$Injection.site.pain = 1}
      if ("Injection site pruritus" %in% symptoms) {test_data$Injection.site.pruritus = 1}
      if ("Injection site swelling" %in% symptoms) {test_data$Injection.site.swelling = 1}
      if ("Injection site warmth" %in% symptoms) {test_data$Injection.site.warmth = 1}
      if ("Lymphadenopathy" %in% symptoms) {test_data$Lymphadenopathy = 1}
      if ("Malaise" %in% symptoms) {test_data$Malaise = 1}
      if ("Myalgia" %in% symptoms) {test_data$Myalgia = 1}
      if ("Nausea" %in% symptoms) {test_data$Nausea = 1}
      if ("Pain" %in% symptoms) {test_data$Pain = 1}
      if ("Pain in extremity" %in% symptoms) {test_data$Pain.in.extremity = 1}
      if ("Paraesthesia" %in% symptoms) {test_data$Paraesthesia = 1}
      if ("Pruritus" %in% symptoms) {test_data$Pruritus = 1}
      if ("Pyrexia" %in% symptoms) {test_data$Pyrexia = 1}
      if ("Rash" %in% symptoms) {test_data$Rash = 1}
      if ("SARS-CoV-2 test negative" %in% symptoms) {test_data$SARS.CoV.2.test.negative = 1}
      if ("SARS-CoV-2 test positive" %in% symptoms) {test_data$SARS.CoV.2.test.positive = 1}
      if ("Urticaria" %in% symptoms) {test_data$Urticaria = 1}
      if ("Vomiting" %in% symptoms) {test_data$Vomiting = 1}
      
      if ("Penicillin" %in% allergies) {test_data$penicillin = 1}
      if ("Sulfa" %in% allergies) {test_data$sulfa = 1}
      
      if ("Allergies" %in% medicalhist) {test_data$allergi = 1}
      if ("Asthma" %in% medicalhist) {test_data$asthma = 1}
      if ("Hypertension" %in% medicalhist) {test_data$hypertens = 1}
      if ("Diabetes" %in% medicalhist) {test_data$diabet = 1}
      if ("Hypothyroid" %in% medicalhist) {test_data$hypothyroid = 1}
      if ("Hypertension" %in% medicalhist) {test_data$blood = 1}
      if ("Hypertension" %in% medicalhist) {test_data$pressur = 1}
      
      prob = predict(classification,test_data,type="prob")
      x = prob[1,2]
      x
  }
  paste(round(100*machinelearning(input$Gender, input$AGE_YRS, input$Vaccine, input$NUMDAYS, input$symptoms, input$allergies, input$medicalhist), 2), "%", sep="")
  })
  
  output$plotcluster1 <- renderPlot({
    test <- read.csv('Cluster_Charts.csv')
    ggplot(test[test$Symptom %in% c('Injection.site.erythema',
                                     'Injection.site.pain',
                                     'Injection.site.pruritus',
                                     'Injection.site.swelling',
                                     'Injection.site.warmth'),],
           aes(fill=Symptom, y=Frequency, x=Cluster)) + 
      geom_bar(position="dodge", stat="identity") + theme_light() + scale_fill_brewer(palette="Reds")
  })
  output$plotcluster2 <- renderPlot({
    test <- read.csv('Cluster_Charts.csv')
    ggplot(test[test$Symptom %in% c('Dizziness',
                                     'Nausea'),],
           aes(fill=Symptom, y=Frequency, x=Cluster)) + 
      geom_bar(position="dodge", stat="identity") + theme_light() + scale_fill_brewer(palette="Reds")
  })
  output$plotcluster3 <- renderPlot({
    test <- read.csv('Cluster_Charts.csv')
    ggplot(test[test$Symptom %in% c('Chills',
                                     'Pyrexia',
                                     'Pain'),],
           aes(fill=Symptom, y=Frequency, x=Cluster)) + 
      geom_bar(position="dodge", stat="identity") + theme_light() + scale_fill_brewer(palette="Reds")
  })
  output$plotcluster4 <- renderPlot({
    test <- read.csv('Cluster_Charts.csv')
    ggplot(test[test$Symptom %in% c('Headache',
                                     'Fatigue'),],
           aes(fill=Symptom, y=Frequency, x=Cluster)) + 
      geom_bar(position="dodge", stat="identity") + theme_light() + scale_fill_brewer(palette="Reds")
  })
  output$predictedcluster <- renderText({
    
    clusterpredic <- function(symptoms) {
      test_data = read.csv("test_data_headers-clusters.csv", header = TRUE)
      load('ClusterModel.rda') #load saved model

      if ("Arthralgia" %in% symptoms) {test_data$Arthralgia = 1}
      if ("Asthenia" %in% symptoms) {test_data$Asthenia = 1}
      if ("COVID-19" %in% symptoms) {test_data$COVID.19 = 1}
      if ("Chills" %in% symptoms) {test_data$Chills = 1}
      if ("Cough" %in% symptoms) {test_data$Cough = 1}
      if ("Diarrhoea" %in% symptoms) {test_data$Diarrhoea = 1}
      if ("Dizziness" %in% symptoms) {test_data$Dizziness = 1}
      if ("Dyspnoea" %in% symptoms) {test_data$Dyspnoea = 1}
      if ("Erythema" %in% symptoms) {test_data$Erythema = 1}
      if ("Fatigue" %in% symptoms) {test_data$Fatigue = 1}
      if ("Feeling abnormal" %in% symptoms) {test_data$Feeling.abnormal = 1}
      if ("Headache" %in% symptoms) {test_data$Headache = 1}
      if ("Hyperhidrosis" %in% symptoms) {test_data$Hyperhidrosis = 1}
      if ("Hypoaesthesia" %in% symptoms) {test_data$Hypoaesthesia = 1}
      if ("Injection site erythema" %in% symptoms) {test_data$Injection.site.erythema = 1}
      if ("Injection site pain" %in% symptoms) {test_data$Injection.site.pain = 1}
      if ("Injection site pruritus" %in% symptoms) {test_data$Injection.site.pruritus = 1}
      if ("Injection site swelling" %in% symptoms) {test_data$Injection.site.swelling = 1}
      if ("Injection site warmth" %in% symptoms) {test_data$Injection.site.warmth = 1}
      if ("Lymphadenopathy" %in% symptoms) {test_data$Lymphadenopathy = 1}
      if ("Malaise" %in% symptoms) {test_data$Malaise = 1}
      if ("Myalgia" %in% symptoms) {test_data$Myalgia = 1}
      if ("Nausea" %in% symptoms) {test_data$Nausea = 1}
      if ("Pain" %in% symptoms) {test_data$Pain = 1}
      if ("Pain in extremity" %in% symptoms) {test_data$Pain.in.extremity = 1}
      if ("Paraesthesia" %in% symptoms) {test_data$Paraesthesia = 1}
      if ("Pruritus" %in% symptoms) {test_data$Pruritus = 1}
      if ("Pyrexia" %in% symptoms) {test_data$Pyrexia = 1}
      if ("Rash" %in% symptoms) {test_data$Rash = 1}
      if ("SARS-CoV-2 test negative" %in% symptoms) {test_data$SARS.CoV.2.test.negative = 1}
      if ("SARS-CoV-2 test positive" %in% symptoms) {test_data$SARS.CoV.2.test.positive = 1}
      if ("Urticaria" %in% symptoms) {test_data$Urticaria = 1}
      if ("Vomiting" %in% symptoms) {test_data$Vomiting = 1}
      
      pred.knn <- get.knnx(cluster$center, test_data, 1)$nn.index[,1]
      pred.knn
    }
    paste('Patient belongs to cluster: ',clusterpredic(input$symptoms),sep="")
  })
}

shinyApp(ui,server)