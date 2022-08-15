# install.packages(tidyverse)
# install.packages(dummies)

library(tidyverse)
library(dummies)
library(smotefamily)
library(corrplot)
library(olsrr)
library(class)

# remember to set to your own working directory before running
setwd("/Users/Straight_As/Documents/UA/MIS/Summer2022/MIS545/GroupProject")

# create tibble from csv file (without taking the log of RecordsLost)
dataBreaches <- read_csv(file = "Balloon Race_ Data Breaches - Prepped.csv", 
                         col_types = "ciiffi",
                         col_names = TRUE)

# Display the dataBreaches tibble on the console
print(dataBreaches)

# Display the structure of dataBreaches tibble
str(dataBreaches)

# Display the summary of dataBreaches tibble
summary(dataBreaches)

# Create the getmode function. Will use the DataSensitivty mode to replace 
# na values in the DataSensitivity feature
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# use the getmode function on the DataSensitivity column
v <- dataBreaches$DataSensitivity

# Calculate the mode using the user function.
result <- getmode(v)
print(result)

#use the mutate function to replace the na values with the mode result of 2.
dataBreaches <- dataBreaches %>% mutate(DataSensitivity = 
                                                ifelse(is.na(DataSensitivity), 
                                                       2, DataSensitivity))


##### add outliers here #####


# normalize the RecordsLost feature by taking log10 of RecordsLost and putting 
# into new column called LogRecordsLost 
dataBreaches <- dataBreaches %>%
  mutate(LogRecordsLost = log10(RecordsLost))
  
# remove RecordsLost column from the tibble 
dataBreaches <- dataBreaches %>%  
  select(-RecordsLost)

# create data frame using normalized tibble
dataBreachesDataFrame <- data.frame(dataBreaches)

# discretize DataSensitivity into Type and store in a new data frame called 
# dataBreachesDataFrame
dataBreachesDataFrame <- dataBreachesDataFrame %>%
  mutate(Type = case_when
         (DataSensitivity == 1 ~ "Email/Online Info", 
          DataSensitivity == 2 ~ "SSN/Personal Details", 
          DataSensitivity == 3 ~ "Credit Card Info", 
          DataSensitivity == 4 ~ "Health/Personal Records",
          TRUE ~ "Full Details"))

# convert data frame back into tibble called dataBreaches with dummy variables
dataBreaches <- as_tibble(dummy.data.frame(data = dataBreachesDataFrame,
                                            names = "Type"))

# create data frame for second dummy code step
dataBreachesDataFrame <- data.frame(dataBreaches)

# dummy code Method column for MaliciousActor 
dataBreachesDataFrame <- dataBreachesDataFrame %>%
  mutate(MaliciousActor = case_when
         (Method == "inside job" ~ 1,
           Method == "hacked" ~ 1,
           Method == "poor security" ~ 1,
           Method == "accidental leak" ~ 0,
           Method == "lost device" ~ 0))

# remove Method column from the data frame
dataBreachesDataFrame <- dataBreachesDataFrame %>%  
  select(-Method)

# convert data frame back into dataBreaches tibble
dataBreaches <- as_tibble(dataBreachesDataFrame)

# Convert Method to Malicious Actor (1 = Malicious Actor & 
# 0 = Non-Malicious Actor) - Jordan

dataBreachesDataFrame3 <-data.frame(dataBreaches3)
dataBreachesDataFrame4 <- dataBreachesDataFrame3 %>%
  mutate(MaliciousActor = case_when
         (Method == "inside job" ~ 1,
           Method == "hacked" ~ 1,
           Method == "poor security" ~ 1,
           Method == "accidental leak" ~ 0,
           Method == "lost device" ~ 0))          
dataBreaches4 <- as_tibble(dummy.data.frame(data = dataBreachesDataFrame4,
                                            names = "Type"))

# Logistic Regression & Corr Plot - Jordan-------------------------------

# dummy sector

dataBreaches5 <- as_tibble(dummy.data.frame(data = dataBreachesDataFrame4,
                                            names = "Sector"))

# Convert columns from dummy to logical

dataBreaches5$Sectorfinancial <- as.logical(dataBreaches5$Sectorfinancial)
dataBreaches5$Sectortech <- as.logical(dataBreaches5$Sectortech)
dataBreaches5$Sectorretail <- as.logical(dataBreaches5$Sectorretail)
dataBreaches5$Sectorgovernment <- as.logical(dataBreaches5$Sectorgovernment)
dataBreaches5$SectorNGO <- as.logical(dataBreaches5$SectorNGO)
dataBreaches5$Sectorweb <- as.logical(dataBreaches5$Sectorweb)
dataBreaches5$Sectormisc <- as.logical(dataBreaches5$Sectormisc)
dataBreaches5$Sectortransport <- as.logical(dataBreaches5$Sectortransport)
dataBreaches5$Sectorlegal <- as.logical(dataBreaches5$Sectorlegal)
dataBreaches5$Sectorgaming <- as.logical(dataBreaches5$Sectorgaming)
dataBreaches5$Sectortelecoms <- as.logical(dataBreaches5$Sectortelecoms)
dataBreaches5$Sectorfinance <- as.logical(dataBreaches5$Sectorfinance)
dataBreaches5$Sectorhealth <- as.logical(dataBreaches5$Sectorhealth)
dataBreaches5$`Sectormisc, health` <-
  as.logical(dataBreaches5$`Sectormisc, health`)
dataBreaches5$`Sectortech, health` <-
  as.logical(dataBreaches5$`Sectortech, health`)
dataBreaches5$Sectoracademic <- as.logical(dataBreaches5$Sectoracademic)
dataBreaches5$`Sectortech, app` <- as.logical(dataBreaches5$`Sectortech, app`)
dataBreaches5$`Sectorweb, tech` <- as.logical(dataBreaches5$`Sectorweb, tech`)
dataBreaches5$`Sectortech, web` <- as.logical(dataBreaches5$`Sectortech, web`)
dataBreaches5$`Sectorgovernment, health` <-
  as.logical(dataBreaches5$`Sectorgovernment, health`)
dataBreaches5$`Sectorweb, military` <-
  as.logical(dataBreaches5$`Sectorweb, military`)
dataBreaches5$`Sectortech, retail` <-
  as.logical(dataBreaches5$`Sectortech, retail`)
dataBreaches5$Sectormilitary <- as.logical(dataBreaches5$Sectormilitary)
dataBreaches5$Sectorapp <- as.logical(dataBreaches5$Sectorapp)
dataBreaches5$`Sectormilitary, health` <-
  as.logical(dataBreaches5$`Sectormilitary, health`)
dataBreaches5$`Sectorweb, gaming` <-
  as.logical(dataBreaches5$`Sectorweb, gaming`)
dataBreaches5$`Sectorgovernment, military` <-
  as.logical(dataBreaches5$`Sectorgovernment, military`)
dataBreaches5$TypeCredit.Card.Info <-
  as.logical(dataBreaches5$TypeCredit.Card.Info)
dataBreaches5$TypeEmail.Online.Info <-
  as.logical(dataBreaches5$TypeEmail.Online.Info)
dataBreaches5$TypeFull.Details <- as.logical(dataBreaches5$TypeFull.Details)
dataBreaches5$TypeHealth.Personal.Records <-
  as.logical(dataBreaches5$TypeHealth.Personal.Records)
dataBreaches5$TypeSSN.Personal.Details <-
  as.logical(dataBreaches5$TypeSSN.Personal.Details)
dataBreaches5$TypeUnknown <- as.logical(dataBreaches5$TypeUnknown)
dataBreaches5$MaliciousActor <- as.logical(dataBreaches5$MaliciousActor)

# create dataframe from logical data

dataBreachesDataFrame6 <- data.frame(dataBreaches5)

# eliminate missing data rows and create new dataframe

dataBreachesDataFrame7 <- dataBreachesDataFrame6 %>%
  filter(!is.na(MaliciousActor)) %>%
  filter(!is.na(LogRecordsLost)) %>%
  filter(!is.na(RecordsLost))

# eliminate uneeded columns and create new dataframe

dataBreachesDataFrame8 <- dataBreachesDataFrame7 %>%
  select(-c(Organization, Year, Sector, Method, DataSensitivity))

# create new tibble with cleaned dataframe

dataBreaches6 <- as_tibble(dataBreachesDataFrame8)

# create a corrplot

round(cor(dataBreaches6), 2)

corrplot(round(cor(dataBreaches6), 2),
         method = "shade",
         type = "lower",
         title = "Corr Plot of Data Breaches - Group 10")

# Create training and testing data sets

set.seed(5654)

sampleSet <- sample(nrow(dataBreaches6),
                    round(nrow(dataBreaches6) * 0.75),
                    replace = FALSE)

dataBreaches6Training <- dataBreaches6[sampleSet, ]

dataBreaches6Testing <- dataBreaches6[-sampleSet, ]

summary(dataBreaches6Training$MaliciousActor)

# fix class imbalance

classImbalanceMagnitude <- 254/56

dataBreaches6TrainingSmoted <-
  tibble(SMOTE(X = data.frame(dataBreaches6Training),
               target = dataBreaches6Training$MaliciousActor,
               dup_size = 4)$data)

summary(dataBreaches6TrainingSmoted)

dataBreaches6TrainingSmoted <- dataBreaches6TrainingSmoted %>%
  mutate(Sectorfinancial = as.logical(Sectorfinancial),
         Sectortech = as.logical(Sectortech),
         Sectorretail = as.logical(Sectorretail),
         Sectorgovernment = as.logical(Sectorgovernment),
         SectorNGO = as.logical(SectorNGO),
         Sectorweb = as.logical(Sectorweb),
         Sectormisc = as.logical(Sectormisc),
         Sectortransport = as.logical(Sectortransport),
         Sectorlegal = as.logical(Sectorlegal),
         Sectorgaming = as.logical(Sectorgaming),
         Sectortelecoms = as.logical(Sectortelecoms),
         Sectorfinance = as.logical(Sectorfinance),
         Sectorhealth = as.logical(Sectorhealth),
         Sectormisc..health = as.logical(Sectormisc..health),
         Sectortech..health = as.logical(Sectortech..health),
         Sectoracademic = as.logical(Sectoracademic),
         Sectortech..app = as.logical(Sectortech..app),
         Sectorweb..tech = as.logical(Sectorweb..tech),
         Sectortech..web = as.logical(Sectortech..web),
         Sectorgovernment..health = as.logical(Sectorgovernment..health),
         Sectorweb..military = as.logical(Sectorweb..military),
         Sectortech..retail = as.logical(Sectortech..retail),
         Sectormilitary = as.logical(Sectormilitary),
         Sectorapp = as.logical(Sectorapp),
         Sectormilitary..health = as.logical(Sectormilitary..health),
         Sectorweb..gaming = as.logical(Sectorweb..gaming),
         Sectorgovernment..military =
           as.logical(Sectorgovernment..military),
         TypeCredit.Card.Info = as.logical(TypeCredit.Card.Info),
         TypeEmail.Online.Info = as.logical(TypeEmail.Online.Info),
         TypeFull.Details = as.logical(TypeFull.Details),
         TypeHealth.Personal.Records = as.logical(TypeHealth.Personal.Records),
         TypeSSN.Personal.Details = as.logical(TypeSSN.Personal.Details),
         TypeUnknown = as.logical(TypeUnknown),
         MaliciousActor = as.logical(MaliciousActor))

dataBreaches6TrainingSmoted <- dataBreaches6TrainingSmoted %>%
  select(-class)

summary(dataBreaches6TrainingSmoted)

# create logistic regression model

dataBreachesLogisticRegressionMaliciousActorModel <-
  glm(data = dataBreaches6TrainingSmoted,
      family = binomial,
      formula = MaliciousActor ~ .)

summary(dataBreachesLogisticRegressionMaliciousActorModel)

exp(coef(dataBreachesLogisticRegressionMaliciousActorModel)["RecordsLost"])

# test logistic regression model

dataBreachesMaliciousActorPrediction <-
  predict(dataBreachesLogisticRegressionMaliciousActorModel,
          dataBreaches6Testing,
          type = "response")

print(dataBreachesMaliciousActorPrediction)

dataBreachesMaliciousActorPrediction <-
  ifelse(dataBreachesMaliciousActorPrediction >= 0.5, 1, 0)

print(dataBreachesMaliciousActorPrediction)

# create confusion matrix

dataBreachesMaliciousActorConfusionMatrix <-
  table(dataBreaches6Testing$MaliciousActor,
        dataBreachesMaliciousActorPrediction)

print(dataBreachesMaliciousActorConfusionMatrix)

# false positive rate

dataBreachesMaliciousActorConfusionMatrix[1, 2] /
  (dataBreachesMaliciousActorConfusionMatrix [1, 2] +
     dataBreachesMaliciousActorConfusionMatrix[1, 1])

# false negative rate

dataBreachesMaliciousActorConfusionMatrix[2, 1] /
  (dataBreachesMaliciousActorConfusionMatrix [2, 1] +
     dataBreachesMaliciousActorConfusionMatrix[2, 2])

# total predictive accuracy of logistic regression model

sum(diag(dataBreachesMaliciousActorConfusionMatrix)) /
  nrow(dataBreaches6Testing)

# END OF LOGISTIC REGRESSION MODEL---------------


# DECISION TREE Michael W -------------------------
# DECISION TREE Michael W  -------------------------
# DECISION TREE Michael W  -------------------------

# Install rpart.plot package
# install.packages("rpart.plot")

# Load rpart.plot packages
library(rpart.plot)


# Remove unnecssary features from the root tibble
dataBreachesDecisionTree <- dataBreaches %>% select(-Organization,-Method)

# Split data intto training and testing
# The set.seed() function is used to ensure that we can get the same result
# every time we run a random sampling process.
set.seed(1589)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSetDecisionTree <- sample(nrow(dataBreachesDecisionTree),
                    round(nrow(dataBreachesDecisionTree) * 0.75),
                    replace = FALSE)

# Put records from 75% sample into dataBreaches Training    
dataBreachesTraining <- dataBreachesDecisionTree[sampleSetDecisionTree, ]

# Put the 25% remaining records into dataBreachesTesting
dataBreachesTesting <- dataBreachesDecisionTree[-sampleSetDecisionTree, ]

# Train the decision tree model using the training data set. Note the complexity
# parameter of 0.01 as the default value.
dataBreachesDecisionTreeModel <- rpart(formula = MaliciousActor ~ .,
                                    method = "class",
                                    cp = 0.05,
                                    data = dataBreachesTraining)

# Display the decision tree plot
rpart.plot(dataBreachesDecisionTreeModel)

# Predict classes for each record in the testindatasets::
dataBreachesPrediction <- predict(dataBreachesDecisionTreeModel,
                                  dataBreachesTesting,
                               type = "class")

# Display the predictions from dataBreachesPrediction to the console
print(dataBreachesPrediction)

# Evaluate the model by forming a confusion matrix
dataBreachesConfusionMatrix <- table(dataBreachesTesting$MaliciousActor,
                                  dataBreachesPrediction)

# Display the dataBreachesConfusionMatrix to the console
print(dataBreachesConfusionMatrix )

# Calculate the model predicitive accuracy
predictiveAccuracy <- sum(diag(dataBreachesConfusionMatrix)) /
  nrow(dataBreachesTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)

# k-Nearest neighbor Model - Jordan----------------------

# clean data for k-nearest neighbors

library(class)

dataBreaches7 <- dataBreaches6 %>%
  select(-RecordsLost)

dataBreachesDataFrame9 <- data.frame(dataBreaches7)

dataBreachesDataFrame9 <- dataBreachesDataFrame9 %>%
  mutate(MaliciousActor = case_when
         (is.na(MaliciousActor) ~ "Unknown",
           MaliciousActor == TRUE ~ "Malicious Actor", 
           MaliciousActor == FALSE ~ "Non-Malicious Actor")) 

dataBreaches7 <- as_tibble(dataBreachesDataFrame9)

dataBreaches7 <- dataBreaches7 %>%
  select(-LogRecordsLost)

# Create Training and Testing Sets

set.seed(5654)

dataBreaches7Labels <- dataBreaches7 %>% select(MaliciousActor)
dataBreaches7 <- dataBreaches7 %>% select(-MaliciousActor)

sampleSet2 <- sample(nrow(dataBreaches7),
                    round(nrow(dataBreaches7) * 0.75),
                    replace = FALSE)

dataBreaches7Training <- dataBreaches6[sampleSet2, ]

# Balance Data with Smote

dataBreaches7TrainingSmoted <-
  tibble(SMOTE(X = data.frame(dataBreaches7Training),
               target = dataBreaches7Training$MaliciousActor,
               dup_size = 4)$data)

summary(dataBreaches7TrainingSmoted)

dataBreaches7TrainingSmoted <- dataBreaches7TrainingSmoted %>%
  mutate(Sectorfinancial = as.logical(Sectorfinancial),
         Sectortech = as.logical(Sectortech),
         Sectorretail = as.logical(Sectorretail),
         Sectorgovernment = as.logical(Sectorgovernment),
         SectorNGO = as.logical(SectorNGO),
         Sectorweb = as.logical(Sectorweb),
         Sectormisc = as.logical(Sectormisc),
         Sectortransport = as.logical(Sectortransport),
         Sectorlegal = as.logical(Sectorlegal),
         Sectorgaming = as.logical(Sectorgaming),
         Sectortelecoms = as.logical(Sectortelecoms),
         Sectorfinance = as.logical(Sectorfinance),
         Sectorhealth = as.logical(Sectorhealth),
         Sectormisc..health = as.logical(Sectormisc..health),
         Sectortech..health = as.logical(Sectortech..health),
         Sectoracademic = as.logical(Sectoracademic),
         Sectortech..app = as.logical(Sectortech..app),
         Sectorweb..tech = as.logical(Sectorweb..tech),
         Sectortech..web = as.logical(Sectortech..web),
         Sectorgovernment..health = as.logical(Sectorgovernment..health),
         Sectorweb..military = as.logical(Sectorweb..military),
         Sectortech..retail = as.logical(Sectortech..retail),
         Sectormilitary = as.logical(Sectormilitary),
         Sectorapp = as.logical(Sectorapp),
         Sectormilitary..health = as.logical(Sectormilitary..health),
         Sectorweb..gaming = as.logical(Sectorweb..gaming),
         Sectorgovernment..military =
           as.logical(Sectorgovernment..military),
         TypeCredit.Card.Info = as.logical(TypeCredit.Card.Info),
         TypeEmail.Online.Info = as.logical(TypeEmail.Online.Info),
         TypeFull.Details = as.logical(TypeFull.Details),
         TypeHealth.Personal.Records = as.logical(TypeHealth.Personal.Records),
         TypeSSN.Personal.Details = as.logical(TypeSSN.Personal.Details),
         TypeUnknown = as.logical(TypeUnknown),
         MaliciousActor = as.logical(MaliciousActor))

dataBreaches7TrainingSmoted <- dataBreaches7TrainingSmoted %>%
  select(-RecordsLost, -LogRecordsLost)

dataBreachesDataFrame10 <- data.frame(dataBreaches7TrainingSmoted)

# Finalize Training Set and Testing Set

dataBreachesDataFrame10 <- dataBreachesDataFrame10 %>%
  mutate(MaliciousActor = case_when
         (is.na(MaliciousActor) ~ "Unknown",
           MaliciousActor == TRUE ~ "Malicious Actor", 
           MaliciousActor == FALSE ~ "Non-Malicious Actor"))

dataBreaches7Training <- as_tibble(dataBreachesDataFrame10)
dataBreaches7Training <- dataBreaches7Training %>% drop_na()

dataBreaches7TrainingLabels <- dataBreaches7Training %>% select(MaliciousActor)
dataBreaches7Training <- dataBreaches7Training %>% select(-MaliciousActor,
                                                          -class)

dataBreaches7Testing <- dataBreaches6[-sampleSet2, ]
dataBreaches7Testing <- dataBreaches7Testing %>%
  select(-RecordsLost, -LogRecordsLost, -MaliciousActor)
dataBreaches7TestingLabels <- dataBreaches7Labels[-sampleSet2, ]

# Create k-Nearest Neighbor Model 
# Updated k value to 115 based on most accurate value in for loop

kNearestMaliciousActorPrediction <-
  knn(train = dataBreaches7Training,
      test = dataBreaches7Testing,
      cl = dataBreaches7TrainingLabels$MaliciousActor,
      k = 115)

print(kNearestMaliciousActorPrediction)
print(summary(kNearestMaliciousActorPrediction))

# Create Confusion Matrix

dataBreaches7ConfusionMatrix <-
  table(dataBreaches7TestingLabels$MaliciousActor,
        kNearestMaliciousActorPrediction)

print(dataBreaches7ConfusionMatrix)

# Check Predictive Accuracy

dataBreaches7PredictiveAccuracy <- sum(diag(dataBreaches7ConfusionMatrix)) /
  nrow(dataBreaches7Testing)

print(dataBreaches7PredictiveAccuracy)

# Find the most accurate value of K

dataBreaches7KValueMatrix <- matrix(data = NA,
                                    nrow = 0,
                                    ncol = 2)

colnames(dataBreaches7KValueMatrix) <- c("k value", "Predictive Accuracy")
for (kValue in 1:nrow(dataBreaches7Training)) {
  if(kValue %% 2 != 0) {
    kNearestMaliciousActorPrediction <- knn(train = dataBreaches7Training,
                               test = dataBreaches7Testing,
                               cl = dataBreaches7TrainingLabels$MaliciousActor,
                               k = kValue)
    dataBreaches7ConfusionMatrix <-
      table(dataBreaches7TestingLabels$MaliciousActor,
            kNearestMaliciousActorPrediction)
    predictiveAccuracy <- sum(diag(dataBreaches7ConfusionMatrix)) /
      nrow(dataBreaches7Testing)
    dataBreaches7KValueMatrix <- rbind(dataBreaches7KValueMatrix,
                                       c(kValue, predictiveAccuracy))
  }
}
print(dataBreaches7KValueMatrix)

#  End of k-Nearest Neighbor Code-------------------------



# begin neural network code - Angela ---------------------

# install.packages("neuralnet")

# Load necessary libraries
library(neuralnet)

# Display the dataBreaches summary
summary(dataBreaches)

# Create GovernmentSector column to discretize Sector into Government and 
# Non-Government
dataBreaches <- dataBreaches %>%
  mutate(GovernmentSector = grepl("government|military", Sector))

# Scale the LogRecordsLost feature from 0 to 1
dataBreaches <- dataBreaches %>%
  mutate(LogRecordsLostScaled = (LogRecordsLost - min(LogRecordsLost)) /
           (max(LogRecordsLost) - min(LogRecordsLost)))

# Scale the Year features from 0 to 1
dataBreaches <- dataBreaches %>%
  mutate(YearScaled = (Year - min(Year)) /
           (max(Year) - min(Year)))

# The set.seed() function is used to ensure that we can get the same result
# every time we run a random sampling process.
set.seed(1234)

# Split the data into training and testing
sampleSetNeuralNet <- sample(nrow(dataBreaches),
                             round(nrow(dataBreaches) * 0.75),
                             replace = FALSE)

# Put the records from the 75% sample into dataBreachesNeuralNetTraining
dataBreachesNeuralNetTraining <- dataBreaches[sampleSetNeuralNet, ]

# Put the records from the 25% sample into dataBreachesNeuralNetTesting
dataBreachesNeuralNetTesting <- dataBreaches[-sampleSetNeuralNet, ]

# Generate the neural network
dataBreachesNeuralNet <- neuralnet(
  formula = MaliciousActor ~ LogRecordsLostScaled + TypeCredit.Card.Info + 
    TypeEmail.Online.Info + TypeFull.Details + TypeHealth.Personal.Records +
    TypeSSN.Personal.Details + Year + GovernmentSector,
  data = dataBreachesNeuralNetTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# Display the neural network results
print(dataBreachesNeuralNet$result.matrix)

# Visualize the neural network
plot(dataBreachesNeuralNet)

# Generate probabilities on the dataBreachesNeuralNetTesting dataset
dataBreachesProbability <- compute(dataBreachesNeuralNet,
                                   dataBreachesNeuralNetTesting)

# Display predictions from the testing dataset on the console
print(dataBreachesProbability$net.result)

# Convert probability predictions into 0/1 predictions
dataBreachesPrediction <-
  ifelse(dataBreachesProbability$net.result > 0.5, 1, 0)

# Display the predictions on the console
print(dataBreachesPrediction)

# Evaluate the model by forming a confusion matrix
dataBreachesConfusionMatrix <- table(dataBreachesNeuralNetTesting$MaliciousActor,
                                     dataBreachesPrediction)

# Display the predictions on the console
print(dataBreachesConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(dataBreachesConfusionMatrix)) /
  nrow(dataBreachesNeuralNetTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)


# end neural network code --------------------------------