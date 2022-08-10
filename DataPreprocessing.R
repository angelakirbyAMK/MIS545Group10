# Perform any necessary data preprocessing.
# Handle any missing data (impute or delete using your best judgment)

# Analyze histograms of all continuous variables and normalize any non-normal features
# Michael Wagenheim test comment

# Scale and/or smooth data if necessary

# Discretize variables if necessary

# Dummy code categorical variables if your dataset contains any

# Identify outliers and handle them to your best judgment

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

