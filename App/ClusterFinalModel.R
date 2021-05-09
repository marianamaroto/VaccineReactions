# COVID-19 Vaccine Machine Learning

# Import dataset 
file_loc <- "C:\\Users\\MarianaMaroto\\Desktop\\MS in Data Science\\74020 Machine Learning\\ML Project\\cleaned_data.csv"
dataset_original <- read.csv(file_loc, header = TRUE)

dataset <- dataset_original[,1:34]
dataset <- subset(dataset, select = -c(X))

set.seed(42)
cluster <- kmeans(dataset, 5, nstart = 42)
# Save Model
save(cluster , file = 'ClusterModel.rda')

# save clusters
dataset_analysis <- dataset
dataset_analysis$cluster <- cluster$cluster
write.csv(dataset_analysis, 'Cluster_Analysis.csv', row.names = FALSE)

library(dplyr)
summary <- dataset_analysis %>% 
  group_by(cluster) %>% 
  summarise_each(list(sum))

library(reshape)
test <- melt(as.data.frame(summary), id.vars = 'cluster')
write.csv(test, 'Cluster_Charts.csv', row.names = FALSE)

library(ggplot2)
# Grouped
ggplot(test[test$variable %in% c('Injection.site.erythema',
                                 'Injection.site.pain',
                                 'Injection.site.pruritus',
                                 'Injection.site.swelling',
                                 'Injection.site.warmth'),],
       aes(fill=variable, y=value, x=cluster)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(test[test$variable %in% c('Dizziness',
                                 'Nausea'),],
       aes(fill=variable, y=value, x=cluster)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(test[test$variable %in% c('Chills',
                                 'Pyrexia',
                                 'Pain'),],
       aes(fill=variable, y=value, x=cluster)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(test[test$variable %in% c('Headache',
                                 'Fatigue'),],
       aes(fill=variable, y=value, x=cluster)) + 
  geom_bar(position="dodge", stat="identity")

# save clusters
dataset_original2 <- dataset_original
dataset_original2$cluster <- cluster$cluster

library(dplyr)
new_df <- dataset_original2 %>%
  group_by(cluster) %>%
  summarise(life_threat_ratio = sum(target)/n(), female_ratio = sum(Gender)/n(),
            age_mean = mean(AGE_YRS), days_mean = mean(NUMDAYS),
            Moderna_ratio = sum(MODERNA)/n(), Pfizer_ratio = sum(PFIZER.BIONTECH)/n(),
            allergies_ratio = sum(allergi)/n(), hypertension_ratio = sum(hypertens)/n())

# predicting your cluster
symptoms = c("Headache")
test_data = read.csv("test_data_headers-clusters.csv", header = TRUE)

library(FNN)

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

