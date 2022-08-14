# Perform any necessary data preprocessing.
# Handle any missing data (impute or delete using your best judgment)

# Analyze histograms of all continuous variables and normalize any non-normal features
# Michael Wagenheim test comment

# Scale and/or smooth data if necessary

# Discretize variables if necessary

# Dummy code categorical variables if your dataset contains any

# Identify outliers and handle them to your best judgment

<<<<<<< HEAD
# Install tidyverseand dummies
# install.packages("tidyverse")
# install.packages("C:/Users/wagenhei/Documents/MIS 545/Lab02/
# dummies/dummies_1.5.6.tar/dummies_1.5.6.tar",
# # repos = NULL, type="source")
# install.packages("dplyr")
# install.packages("tidyr")

# Load tidyverse and dummies
library(tidyverse)
library(dummies)
library(dplyr)
library(tidyr)

# Set working directory
setwd("C:/Users/wagenhei/Documents/MIS 545/Group Project/MIS545Group10GitHub/MISGroup10")

# Read Balloon Race_ Data Breaches - Prepped.csv into a tibble called breachData
# l for logical
# n for numeric
# i for integer
# c for character
# f for factor
# D for date
# T for datetime
breachData <- read_csv(file = "Balloon Race_ Data Breaches - Prepped.csv",
                       col_types = "ciiffi",
                       col_names = TRUE)

# Display the breachData tibble on the console
print(breachData)

# Display the structure of breachData tibble
str(breachData)

# Display the summary of breachData tibble
summary(breachData)


# Create the getmode function. Will use the DataSensitivty mode to replace 
# na values in the DataSensitivity feature
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create the vector with feature numbers.
v <- c(1,2,3,4,5)

# Calculate the mode using the user function.
result <- getmode(v)
print(result)

#convert tibble to a data frame so the mutate function can be used to replace 
# the na values with the mode result of 1.
breachDataFrame <- as.data.frame(breachData)

#view class of breachDataFrame
class(breachDataFrame)

#view data frame
print(breachDataFrame)

# Use the mutate function to replace the na value.
breachDataFrame <- breachDataFrame %>% mutate(DataSensitivity = 
                                                ifelse(is.na(DataSensitivity), 
                                                       1, DataSensitivity))

# Convert breachDataFrame back to beachData tibble with no na values
breachData <- as_tibble(breachDataFrame)


# Calculate outliermin and outliermax
OutlierMin <- quantile(breachData$DataSensitivity, .25) -
  (IQR(breachData$DataSensitivity) * 1.5)
OutlierMax <- quantile(breachData$DataSensitivity, .75) +
  (IQR(breachData$DataSensitivity) * 1.5)

=======
# Calculate any new variables if necessary with mutate()



# install.packages(tidyverse)
# install.packages(dummies)

library(tidyverse)
library(dummies)
library(olsrr)

# remember to set to your own working directory before running
setwd("/Users/Straight_As/Documents/UA/MIS/Summer2022/MIS545/GroupProject")

# create tibble from csv file (without taking the log of RecordsLost)
dataBreaches <- read_csv(file = "Balloon Race_ Data Breaches - Prepped.csv", 
                         col_types = "ciiffi",
                         col_names = TRUE)

# normalize the RecordsLost feature by taking log10 of RecordsLost and putting 
# into a new tibble called dataBreaches2 with new column called LogRecordsLost 
dataBreaches2 <- dataBreaches %>%
  mutate(LogRecordsLost = log10(RecordsLost))

# create data frame using normalized tibble
dataBreachesDataFrame <- data.frame(dataBreaches2)

# discretize DataSensitivity into Type and store in a new data frame called 
# dataBreachesDataFrame2
dataBreachesDataFrame2 <- dataBreachesDataFrame %>%
  mutate(Type = case_when
         (is.na(DataSensitivity) ~ "Unknown",
           DataSensitivity == 1 ~ "Email/Online Info", 
           DataSensitivity == 2 ~ "SSN/Personal Details", 
           DataSensitivity == 3 ~ "Credit Card Info", 
           DataSensitivity == 4 ~ "Health/Personal Records",
           TRUE ~ "Full Details"))

# convert data frame back into tibble called dataBreaches3 with dummy variables
dataBreaches3 <- as_tibble(dummy.data.frame(data = dataBreachesDataFrame2,
                                            names = "Type"))
<<<<<<< HEAD
>>>>>>> 01d8f5d52e1858067bc6bdc42cc4a6c0de94bc85
=======

# Convert Method to Malicious Actor (1 = Malicious Actor & 
# 0 = Non-Malicious Actor)

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

# Linear Regression Model - Jordan

dataBreachesModel <- lm(data = dataBreaches4,
                        formula = MaliciousActor ~ RecordsLost + Sector + 
                          LogRecordsLost + TypeCredit.Card.Info + 
                          TypeEmail.Online.Info + TypeFull.Details + 
                          TypeHealth.Personal.Records +
                          TypeSSN.Personal.Details + TypeUnknown)
print(dataBreachesModel)
summary(dataBreachesModel)
ols_vif_tol(dataBreachesModel)
>>>>>>> afe7d616d5349b0a71a19f0a6d5ceb5767c93955
