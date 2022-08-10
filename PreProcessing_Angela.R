#install.packages(tidyverse)
#install.packages(dummies)
#install.packages("olsrr")
#install.packages("corrplot")

library(tidyverse)
library(dummies)
library(olsrr)
library(corrplot)

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

# convert data frame back into tibble called dataBreaches3
dataBreaches3 <- as_tibble(dummy.data.frame(data = dataBreachesDataFrame2,
                                         names = "Type"))
dataBreach3Model <- lm(data = dataBreaches3,
                       formula = Sector ~ .)

print(dataBreach3Model)

ols_vif_tol(dataBreach3Model)

cor(dataBreaches3%>%keep(is.numeric))

Corrplot(cor(dataBreaches3),
             Method = "circle",
             Type = "lower")
    
    