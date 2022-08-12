# Perform any necessary data preprocessing.
# Handle any missing data (impute or delete using your best judgment)

# Analyze histograms of all continuous variables and normalize any non-normal features
# Michael Wagenheim test comment

# Scale and/or smooth data if necessary

# Discretize variables if necessary

# Dummy code categorical variables if your dataset contains any

# Identify outliers and handle them to your best judgment

# Calculate any new variables if necessary with mutate()



# install.packages(tidyverse)
# install.packages(dummies)

library(tidyverse)
library(dummies)

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
                                            names = "Type"))]

# Convert Method to Malicious Actor (1 = Malicious Actor & 0 = Non-Malicious Actor)

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
