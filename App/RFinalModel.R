# COVID-19 Vaccine Machine Learning

# Import dataset 
file_loc <- "C:\\Users\\MarianaMaroto\\Desktop\\MS in Data Science\\74020 Machine Learning\\ML Project\\cleaned_data.csv"
dataset_original <- read.csv(file_loc, header = TRUE)

dataset_original$target <- factor(dataset_original$target)
dataset = subset(dataset_original, select = -c(X))

#Balance Dataset
library(DMwR)
balanced.data <- SMOTE(target ~., dataset, perc.over = 200, k = 5, perc.under = 200)

# Fitting Random Forest to the Training set With default parameters
library(randomForest)
classification = randomForest(x = subset(balanced.data, select = -c(target)),
                              y = balanced.data$target,
                              ntree = 50)

# Save Model
save(classification , file = 'ClassificationModel.rda')

Gender = 'Female'
AGE_YRS = 50
Vaccine = 'Moderna'
NUMDAYS = 2
symptoms = c("Nausea","Pain")
allergies = c("Penicillin","Sulfa")
medicalhist = c("Allergies","Asthma")

test_data = read.csv("test_data_headers.csv", header = TRUE)

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

load('ClassificationModel.rda') #load saved model

prob = predict(classification,test_data,type="prob")
x = prob[1,2]
