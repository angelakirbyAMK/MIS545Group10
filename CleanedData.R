# Naming convention for model specific tibbles and dataframes
# dataBreachesXY
# For X:
# NN = Neural Network
# LR = Logisitc Regression
# DT = Decision Tree
# NB = Naive Bayes
# KNN = K Nearest Neighbor
# For Y:
# Additional description as required.

# install.packages("tidyverse")
# install.packages("dummies")
# install.packages("smotefamily")
# install.packages ("corrplot")
# install.packages("oslrr")
# install.packages("class")
# install.packages("neuralnet")
# install.packages("rpart")
# install.packages("rpart.plot")

library(tidyverse)
library(dummies)
library(smotefamily)
library(corrplot)
library(olsrr)
library(class)
library(neuralnet)
library(rpart)
library(rpart.plot)

# remember to set to your own working directory before running
setwd("/Users/Straight_As/Documents/UA/MIS/Summer2022/MIS545/GroupProject")

# create tibble from csv file
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

# normalize the RecordsLost feature by taking log10 of RecordsLost and putting 
# into new column called LogRecordsLost 
dataBreaches <- dataBreaches %>%
  mutate(LogRecordsLost = log10(RecordsLost))
  
# remove RecordsLost column from the tibble 
dataBreaches <- dataBreaches %>%  
  select(-RecordsLost)

# display summary of tibble on the console
summary(dataBreaches)

# determine outliers in LogRecordsLost feature
# calculate outlier min and max and store into variables called outlierMin and 
# outlierMax
outlierMin <- quantile(dataBreaches$LogRecordsLost, .25) -
  (IQR(dataBreaches$LogRecordsLost) * 1.5)
outlierMax <- quantile(dataBreaches$LogRecordsLost, .75) +
  (IQR(dataBreaches$LogRecordsLost) * 1.5)

# Remove outliers from the dataset (include only non-outliers)
dataBreaches <- dataBreaches %>%
  filter(LogRecordsLost >= outlierMin & LogRecordsLost <= outlierMax)

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

#### Create Multicollinearity Pre-Processing Tibble ---------------------

dataBreachesMulticollinearity <- as_tibble(dataBreachesDataFrame)

dataBreachesDataFrameMulticollinearity <-
  data.frame(dataBreachesMulticollinearity)

# dummy code Method column for MaliciousActor 
dataBreachesDataFrameMulticollinearity <-
  dataBreachesDataFrameMulticollinearity %>%
  mutate(MaliciousActor = case_when
         (Method == "inside job" ~ 1,
           Method == "hacked" ~ 1,
           Method == "poor security" ~ 1,
           Method == "accidental leak" ~ 0,
           Method == "lost device" ~ 0))

# remove unnecessary columns from the data frame
dataBreachesDataFrameMulticollinearity <-
  dataBreachesDataFrameMulticollinearity %>%
  select(-Method, -Organization, -DataSensitivity)

# convert data frame back into dataBreaches tibble
dataBreachesMulticollinearity <-
  as_tibble(dataBreachesDataFrameMulticollinearity)

#### End of Multicollinearity Pre-Processing

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

# remove unnecessary columns from the data frame
dataBreachesDataFrame <- dataBreachesDataFrame %>%
  select(-Method, -Organization, -DataSensitivity)

# convert data frame back into dataBreaches tibble
dataBreaches <- as_tibble(dataBreachesDataFrame)

# display summary of tibble on the console
summary(dataBreaches)

#### end of preprocessing ----------------------------------------------

#### Test for Multicollinearity ---------------------------------------

# Check for Multicollinearity
dataBreachesLinearModel <- lm(data = dataBreachesMulticollinearity,
                        formula = MaliciousActor ~ LogRecordsLost + Sector + 
                          Year + Type)

# Display linear model
print(dataBreachesLinearModel)

# Show summary of linear model
summary(dataBreachesLinearModel)

# Test for Multicollinearity
ols_vif_tol(dataBreachesLinearModel)

#### End of Multicollinearity code -----------------------------------

#### three interesting queries -----------------------------------------

# Query1 - Has the number of data breach events with over 30,000 compromised 
# records increased from the year 2004 to 2021?
query1 <- dataBreaches %>%
  filter(Year < 2022) %>%
  group_by(Year) %>%
  count()

# create line graph graph of query 1
ggplot(query1, aes(x = Year, y = n)) +
  geom_line(color = "red", size = 2) +
  ylab("Count of Events") +
  theme_bw()

# Query 2 - On average, did non-malicious attacks result in more or less 
# compromised records than malicious attacks?
query2 <- dataBreaches %>%
  group_by(MaliciousActor) %>%
  summarize(MinRecordsLost = min(LogRecordsLost),
            MaxRecordsLost = max(LogRecordsLost),
            MedianRecordsLost = median(LogRecordsLost),
            AvgRecordsLost = mean(LogRecordsLost))

# Query 3 - in progress

# end of queries -------------------------------------------------------

#### Logistic Regression Start of Code (Jordan) ------------------------

# use dummy function for sector columns
dataBreachesLR <- as_tibble(dummy.data.frame(data = dataBreachesDataFrame,
                                            names = "Sector"))

# Convert columns from dummy (1 & 0) to logical
dataBreachesLR$Sectorfinancial <- as.logical(dataBreachesLR$Sectorfinancial)
dataBreachesLR$Sectortech <- as.logical(dataBreachesLR$Sectortech)
dataBreachesLR$Sectorretail <- as.logical(dataBreachesLR$Sectorretail)
dataBreachesLR$Sectorgovernment <- as.logical(dataBreachesLR$Sectorgovernment)
dataBreachesLR$SectorNGO <- as.logical(dataBreachesLR$SectorNGO)
dataBreachesLR$Sectorweb <- as.logical(dataBreachesLR$Sectorweb)
dataBreachesLR$Sectormisc <- as.logical(dataBreachesLR$Sectormisc)
dataBreachesLR$Sectortransport <- as.logical(dataBreachesLR$Sectortransport)
dataBreachesLR$Sectorlegal <- as.logical(dataBreachesLR$Sectorlegal)
dataBreachesLR$Sectorgaming <- as.logical(dataBreachesLR$Sectorgaming)
dataBreachesLR$Sectortelecoms <- as.logical(dataBreachesLR$Sectortelecoms)
dataBreachesLR$Sectorfinance <- as.logical(dataBreachesLR$Sectorfinance)
dataBreachesLR$Sectorhealth <- as.logical(dataBreachesLR$Sectorhealth)
dataBreachesLR$`Sectormisc, health` <-
  as.logical(dataBreachesLR$`Sectormisc, health`)
dataBreachesLR$`Sectortech, health` <-
  as.logical(dataBreachesLR$`Sectortech, health`)
dataBreachesLR$Sectoracademic <- as.logical(dataBreachesLR$Sectoracademic)
dataBreachesLR$`Sectortech, app` <- as.logical(dataBreachesLR$`Sectortech, app`)
dataBreachesLR$`Sectorweb, tech` <- as.logical(dataBreachesLR$`Sectorweb, tech`)
dataBreachesLR$`Sectortech, web` <- as.logical(dataBreachesLR$`Sectortech, web`)
dataBreachesLR$`Sectorgovernment, health` <-
  as.logical(dataBreachesLR$`Sectorgovernment, health`)
dataBreachesLR$`Sectorweb, military` <-
  as.logical(dataBreachesLR$`Sectorweb, military`)
dataBreachesLR$`Sectortech, retail` <-
  as.logical(dataBreachesLR$`Sectortech, retail`)
dataBreachesLR$Sectormilitary <- as.logical(dataBreachesLR$Sectormilitary)
dataBreachesLR$Sectorapp <- as.logical(dataBreachesLR$Sectorapp)
dataBreachesLR$`Sectormilitary, health` <-
  as.logical(dataBreachesLR$`Sectormilitary, health`)
dataBreachesLR$`Sectorweb, gaming` <-
  as.logical(dataBreachesLR$`Sectorweb, gaming`)
dataBreachesLR$`Sectorgovernment, military` <-
  as.logical(dataBreachesLR$`Sectorgovernment, military`)
dataBreachesLR$TypeCredit.Card.Info <-
  as.logical(dataBreachesLR$TypeCredit.Card.Info)
dataBreachesLR$TypeEmail.Online.Info <-
  as.logical(dataBreachesLR$TypeEmail.Online.Info)
dataBreachesLR$TypeFull.Details <- as.logical(dataBreachesLR$TypeFull.Details)
dataBreachesLR$TypeHealth.Personal.Records <-
  as.logical(dataBreachesLR$TypeHealth.Personal.Records)
dataBreachesLR$TypeSSN.Personal.Details <-
  as.logical(dataBreachesLR$TypeSSN.Personal.Details)
dataBreachesLR$MaliciousActor <- as.logical(dataBreachesLR$MaliciousActor)

# create dataframe from logical data
dataBreachesDataFrameLR <- data.frame(dataBreachesLR)

# eliminate missing data rows and create new dataframe
dataBreachesDataFrameLR <- dataBreachesDataFrameLR %>%
  filter(!is.na(MaliciousActor)) %>%
  filter(!is.na(LogRecordsLost))

# create new tibble with cleaned dataframe
dataBreachesLR <- as_tibble(dataBreachesDataFrameLR)

# Create a corrplot
corrplot(round(cor(dataBreachesLR), 2),
         method = "shade",
         type = "lower",
         title = "Corr Plot of Data Breaches - Group 10")

# Set seed
set.seed(5654)

# Create sample set
sampleSet <- sample(nrow(dataBreachesLR),
                    round(nrow(dataBreachesLR) * 0.75),
                    replace = FALSE)

# Create Training Set 
dataBreachesLRTraining <- dataBreachesLR[sampleSet, ]

# Create Testing Set
dataBreachesLRTesting <- dataBreachesLR[-sampleSet, ]

# Show summary of Malicious Actor feature
summary(dataBreachesLRTraining$MaliciousActor)

# Locate ideal smote dup size
classImbalanceMagnitude <- 259/51

# Use smote on data to correct class imbalance of malicious actor
dataBreachesLRTrainingSmoted <-
  tibble(SMOTE(X = data.frame(dataBreachesLRTraining),
               target = dataBreachesLRTraining$MaliciousActor,
               dup_size = 4)$data)

# Show summary of smoted training set
summary(dataBreachesLRTrainingSmoted)

# Change all sector and type features to logical
dataBreachesLRTrainingSmoted <- dataBreachesLRTrainingSmoted %>%
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
         MaliciousActor = as.logical(MaliciousActor))

# Remove class feature
dataBreachesLRTrainingSmoted <- dataBreachesLRTrainingSmoted %>%
  select(-class)

# Show summary of training smoted data
summary(dataBreachesLRTrainingSmoted)

# Create logistic regression model
dataBreachesLogisticRegressionMaliciousActorModel <-
  glm(data = dataBreachesLRTrainingSmoted,
      family = binomial,
      formula = MaliciousActor ~ .)

# Show summary of logistic regression model
summary(dataBreachesLogisticRegressionMaliciousActorModel)

# Show the probability of the most significant independent variable "Year"
exp(coef(dataBreachesLogisticRegressionMaliciousActorModel)["Year"])

# Test logistic regression model
dataBreachesLRMaliciousActorPrediction <-
  predict(dataBreachesLogisticRegressionMaliciousActorModel,
          dataBreachesLRTesting,
          type = "response")

# Display result of test
print(dataBreachesLRMaliciousActorPrediction)

# Display result of test in 1 / 0
dataBreachesLRMaliciousActorPrediction <-
  ifelse(dataBreachesLRMaliciousActorPrediction >= 0.5, 1, 0)
print(dataBreachesLRMaliciousActorPrediction)

# create confusion matrix for logistic regression model
dataBreachesLRMaliciousActorConfusionMatrix <-
  table(dataBreachesLRTesting$MaliciousActor,
        dataBreachesLRMaliciousActorPrediction)

# Display confusion matrix
print(dataBreachesLRMaliciousActorConfusionMatrix)

# Display false positive rate
dataBreachesLRMaliciousActorConfusionMatrix[1, 2] /
  (dataBreachesLRMaliciousActorConfusionMatrix [1, 2] +
     dataBreachesLRMaliciousActorConfusionMatrix[1, 1])

# Display false negative rate
dataBreachesLRMaliciousActorConfusionMatrix[2, 1] /
  (dataBreachesLRMaliciousActorConfusionMatrix [2, 1] +
     dataBreachesLRMaliciousActorConfusionMatrix[2, 2])

# Display total predictive accuracy of logistic regression model
sum(diag(dataBreachesLRMaliciousActorConfusionMatrix)) /
  nrow(dataBreachesLRTesting)

# END OF LOGISTIC REGRESSION MODEL---------------

#  DECISION TREE Michael W  -------------------------

# Split data intto training and testing
# The set.seed() function is used to ensure that we can get the same result
# every time we run a random sampling process.
set.seed(1589)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSetDT <- sample(nrow(dataBreaches),
                      round(nrow(dataBreaches) * 0.75),
                      replace = FALSE)

# Put records from 75% sample into dataBreaches Training    
dataBreachesTrainingDT <- dataBreaches[sampleSetDT, ]

# Put the 25% remaining records into dataBreachesTesting
dataBreachesTestingDT <- dataBreaches[-sampleSetDT, ]

# Train the decision tree model using the training data set. Note the complexity
# parameter of 0.01 as the default value.
dataBreachesModelDT <- rpart(formula = MaliciousActor ~ .,
                             method = "class",
                             cp = 0.05,
                             data = dataBreachesTrainingDT)

# Display the decision tree plot
rpart.plot(dataBreachesModelDT)

# Predict classes for each record in the testindatasets::
dataBreachesPredictionDT <- predict(dataBreachesModelDT,
                                    dataBreachesTestingDT,
                                    type = "class")

# Display the predictions from dataBreachesPrediction to the console
print(dataBreachesPredictionDT)

# Evaluate the model by forming a confusion matrix
dataBreachesConfusionMatrixDT <- table(dataBreachesTestingDT$MaliciousActor,
                                       dataBreachesPredictionDT)

# Display the dataBreachesConfusionMatrix to the console
print(dataBreachesConfusionMatrixDT)

# Calculate the model predicitive accuracy
predictiveAccuracyDT <- sum(diag(dataBreachesConfusionMatrixDT)) /
  nrow(dataBreachesTestingDT)

# Display the predictive accuracy on the console
print(predictiveAccuracyDT)

# End Decision Tree Model -----------------------------

# k-Nearest neighbor Model - Jordan----------------------

# clean data for k-nearest neighbors

# Convert MaliciousActor Feature to prepare labels
dataBreachesKNNDataFrame <- dataBreachesDataFrame %>%
  mutate(MaliciousActor = case_when
         (is.na(MaliciousActor) ~ "Unknown",
           MaliciousActor == TRUE ~ "Malicious Actor", 
           MaliciousActor == FALSE ~ "Non-Malicious Actor")) 

# Use dummy function on sector
dataBreachesKNN <- as_tibble(dummy.data.frame(data = dataBreachesKNNDataFrame,
                                              names = "Sector"))
# Remove empty sector column
dataBreachesKNN <- dataBreachesKNN %>%
  select(-Sector)

# Set seed
set.seed(5654)

# Remove rows with missing data
dataBreachesKNN <- dataBreachesKNN %>% drop_na()

# Create tibble with labels and tibble with data
dataBreachesKNNLabels <- dataBreachesKNN %>% select(MaliciousActor)
dataBreachesKNN <- dataBreachesKNN %>% select(-MaliciousActor)

# Create sample set
sampleSetKNN <- sample(nrow(dataBreachesKNN),
                     round(nrow(dataBreachesKNN) * 0.75),
                     replace = FALSE)

# Create training tibbles
dataBreachesKNNTraining <- dataBreachesKNN[sampleSetKNN, ]
dataBreachesKNNTrainingLabels <- dataBreachesKNNLabels[sampleSetKNN, ]

# Create testing tibbles
dataBreachesKNNTesting <- dataBreachesKNN[-sampleSetKNN, ]
dataBreachesKNNTestingLabels <- dataBreachesKNNLabels[-sampleSetKNN, ]

# Create k-Nearest Neighbor Model 
# Updated k value to 3 based on most accurate value in for loop
kNearestMaliciousActorPrediction <-
  knn(train = dataBreachesKNNTraining,
      test = dataBreachesKNNTesting,
      cl = dataBreachesKNNTrainingLabels$MaliciousActor,
      k = 3)

# Display results of K Nearest Neighbor Model
print(kNearestMaliciousActorPrediction)
print(summary(kNearestMaliciousActorPrediction))

# Create Confusion Matrix
dataBreachesKNNConfusionMatrix <-
  table(dataBreachesKNNTestingLabels$MaliciousActor,
        kNearestMaliciousActorPrediction)

# Display confusion Matrix
print(dataBreachesKNNConfusionMatrix)

# Check Predictive Accuracy
dataBreachesKNNPredictiveAccuracy <- sum(diag(dataBreachesKNNConfusionMatrix)) /
  nrow(dataBreachesKNNTesting)

# Display predictive accuracy
print(dataBreachesKNNPredictiveAccuracy)

# Find the most accurate value of K.  1st step is to create K Value Matrix
dataBreachesKNNKValueMatrix <- matrix(data = NA,
                                    nrow = 0,
                                    ncol = 2)

# Build K Value Matrix with for loop
colnames(dataBreachesKNNKValueMatrix) <- c("k value", "Predictive Accuracy")
for (kValue in 1:nrow(dataBreachesKNNTraining)) {
  if(kValue %% 2 != 0) {
    kNearestMaliciousActorPrediction <-
      knn(train = dataBreachesKNNTraining,
          test = dataBreachesKNNTesting,
          cl = dataBreachesKNNTrainingLabels$MaliciousActor,
          k = kValue)
    dataBreachesKNNConfusionMatrix <-
      table(dataBreachesKNNTestingLabels$MaliciousActor,
            kNearestMaliciousActorPrediction)
    predictiveAccuracyKNN <- sum(diag(dataBreachesKNNConfusionMatrix)) /
      nrow(dataBreachesKNNTesting)
    dataBreachesKNNKValueMatrix <- rbind(dataBreachesKNNKValueMatrix,
                                       c(kValue, predictiveAccuracyKNN))
  }
}

# Display K Value Matrix results
print(dataBreachesKNNKValueMatrix)

#  End of k-Nearest Neighbor Code (Jordan) -------------------------



# begin neural network code - Angela ---------------------

# Display the dataBreaches summary
summary(dataBreaches)

# Create GovernmentSector column to discretize Sector into government 
# (including military) and non-government
dataBreachesNN <- dataBreaches %>%
  mutate(GovernmentSector = grepl("government|military", Sector))

# Scale the LogRecordsLost feature from 0 to 1
dataBreachesNN <- dataBreachesNN %>%
  mutate(LogRecordsLostScaled = (LogRecordsLost - min(LogRecordsLost)) /
           (max(LogRecordsLost) - min(LogRecordsLost)))

# Scale the Year features from 0 to 1
dataBreachesNN <- dataBreachesNN %>%
  mutate(YearScaled = (Year - min(Year)) /
           (max(Year) - min(Year)))

# The set.seed() function is used to ensure that we can get the same result
# every time we run a random sampling process.
set.seed(1234)

# Split the data into training and testing
sampleSetNN <- sample(nrow(dataBreachesNN),
                             round(nrow(dataBreachesNN) * 0.75),
                             replace = FALSE)

# Put the records from the 75% sample into dataBreachesTrainingNN
dataBreachesTrainingNN <- dataBreachesNN[sampleSetNN, ]

# Put the records from the 25% sample into dataBreachesTestingNN
dataBreachesTestingNN <- dataBreachesNN[-sampleSetNN, ]

# Generate the neural network
dataBreachesNeuralNet <- neuralnet(
  formula = MaliciousActor ~ LogRecordsLostScaled + TypeCredit.Card.Info + 
    TypeEmail.Online.Info + TypeFull.Details + TypeHealth.Personal.Records +
    TypeSSN.Personal.Details + YearScaled + GovernmentSector,
  data = dataBreachesTrainingNN,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# Display the neural network results
print(dataBreachesNeuralNet$result.matrix)

# Visualize the neural network
plot(dataBreachesNeuralNet)

# Generate probabilities on the dataBreachesTestingNN dataset
dataBreachesProbabilityNN <- compute(dataBreachesNeuralNet,
                                   dataBreachesTestingNN)

# Display predictions from the testing dataset on the console
print(dataBreachesProbabilityNN$net.result)

# Convert probability predictions into 0/1 predictions
dataBreachesPredictionNN <-
  ifelse(dataBreachesProbabilityNN$net.result > 0.5, 1, 0)

# Display the predictions on the console
print(dataBreachesPredictionNN)

# Evaluate the model by forming a confusion matrix
dataBreachesConfusionMatrixNN <- table(dataBreachesTestingNN$MaliciousActor,
                                     dataBreachesPredictionNN)

# Display the predictions on the console
print(dataBreachesConfusionMatrixNN)

# Calculate the model predictive accuracy
predictiveAccuracyNN <- sum(diag(dataBreachesConfusionMatrixNN)) /
  nrow(dataBreachesTestingNN)

# Display the predictive accuracy on the console
print(predictiveAccuracyNN)


# end neural network code --------------------------------

#NAIVE BAYES -------------------------------------------

# install.packages(tidyverse)
# install.packages(dummies)
# install.packages("e1071")

library(tidyverse)
library(dummies)
library(corrplot)
library(olsrr)
library(class)
library(e1071)

# remember to set to your own working directory before running
setwd("~/NaiveBayes")

# create tibble from csv file (without taking the log of RecordsLost)
dataBreachesNB <- read_csv(file = "NaiveBayes.csv", 
                         col_types = "ciiffi",
                         col_names = TRUE)

# Display the dataBreaches tibble on the console
print(dataBreachesNB)

# Display the structure of dataBreaches tibble
str(dataBreachesNB)

# Display the summary of dataBreaches tibble
summary(dataBreachesNB)

# Create the getmode function. Will use the DataSensitivty mode to replace 
# na values in the DataSensitivity feature
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# use the getmode function on the DataSensitivity column
v <- dataBreachesNB$DataSensitivity

# Calculate the mode using the user function.
result <- getmode(v)
print(result)

#use the mutate function to replace the na values with the mode result of 2.
dataBreachesNB <- dataBreachesNB %>% mutate(DataSensitivity = 
                                          ifelse(is.na(DataSensitivity), 
                                                 2, DataSensitivity))


##### add outliers here #####

# create data frame using normalized tibble
dataBreachesDataFrameNB <- data.frame(dataBreachesNB)

# discretize DataSensitivity into Type and store in a new data frame called 
# dataBreachesDataFrame
dataBreachesDataFrameNB <- dataBreachesDataFrameNB %>%
  mutate(Type = case_when
         (DataSensitivity == 1 ~ "Email/Online Info", 
           DataSensitivity == 2 ~ "SSN/Personal Details", 
           DataSensitivity == 3 ~ "Credit Card Info", 
           DataSensitivity == 4 ~ "Health/Personal Records",
           TRUE ~ "Full Details"))

# convert data frame back into tibble called dataBreaches with dummy variables
dataBreachesNB <- as_tibble(dummy.data.frame(data = dataBreachesDataFrameNB,
                                           names = "Type"))

# create data frame for second dummy code step
dataBreachesDataFrameNB <- data.frame(dataBreachesNB)

# dummy code Method column for MaliciousActor 
dataBreachesDataFrameNB <- dataBreachesDataFrameNB %>%
  mutate(MaliciousActor = case_when
         (Method == "inside job" ~ 1,
           Method == "hacked" ~ 1,
           Method == "poor security" ~ 1,
           Method == "accidental leak" ~ 0,
           Method == "lost device" ~ 0))

# remove unnecessary columns from the data frame
dataBreachesDataFrameNB <- dataBreachesDataFrameNB %>%
  select(-Method, -Organization, -DataSensitivity)

# convert data frame back into dataBreaches tibble
dataBreachesNB <- as_tibble(dataBreachesDataFrameNB)

#### end of preprocessing ----------------------------------------------

#NAIVE BAYES

#Look at current data set - first 6
head(dataBreachesNB)

#Check summary for LogRecordsLost mean
summary(dataBreachesNB)

#Split data into training and testing data sets - first set sed
set.seed(1234)

#Create a vector of 75% randomly sampled rows from the original data set
sampleSet <- sample(nrow(dataBreachesNB),
                    round(nrow(dataBreachesNB) * 0.75),
                    replace = FALSE)

#Put the records from the 75% sample into a training set
dataBreachesNaiveBayesTraining <- dataBreachesNB[sampleSet, ]

#Put the records from the other 25% of records into a testing set
dataBreachesNaiveBayesTesting <- dataBreachesNB[-sampleSet, ]

# Train the naïve bayes model (variety is thing being predicted)
dataBreachesNaiveBayesModel <- naiveBayes(formula = MaliciousActor ~ .,
                                          data = dataBreachesNaiveBayesTraining,
                                          laplace = 1)

#Build probabilities for each record in the testing dataset
dataBreachesNaiveBayesProbability <- predict(dataBreachesNaiveBayesModel,
                                             dataBreachesNaiveBayesTesting,
                                             Type = "raw")

#Display the probability from dataBreachesNaiveBayesProbability on the console
print(dataBreachesNaiveBayesProbability)

#Predict classes for each record in the testing dataset
dataBreachesNaiveBayesPrediction <- predict(dataBreachesNaiveBayesModel,
                                            dataBreachesNaiveBayesTesting,
                                            Type = "class")

#Display the predictions from dataBreachesNaiveBayesPrediction on the console
print(dataBreachesNaiveBayesPrediction)

#Evaluate the model by forming a confusion matrix
dataBreachesNaiveBayesConfusionMatrix <- 
  table(dataBreachesNaiveBayesTesting$MaliciousActor,
        dataBreachesNaiveBayesPrediction)

#Display the confusion matrix
print(dataBreachesNaiveBayesConfusionMatrix)

#Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(dataBreachesNaiveBayesConfusionMatrix)) / 
  nrow(dataBreachesNaiveBayesTesting)

#Display predictive accuracy
print(predictiveAccuracy)


#END NAIVE BAYES------------------------------------------------------

#Histogram for main data set........(Note - only 3 of 4 independent variable show (Sector is not numeric)) ----------------------------------

displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                              color = "black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal()
}

displayAllHistograms(dataBreaches)

#END Histogram for data set-----------------------------------------------------------
