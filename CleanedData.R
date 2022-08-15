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