#### Preamble ####
# Purpose: Tests... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(httr)
library(jsonlite)
library(usethis)
library(readr)
library(devtools)
library(kaggler)

#### Test data ####

## import datasets

analysis_data <- read_csv("/cloud/project/data/analysis_data/verstappen_qualifying.csv")

all_qual <- read_csv("/cloud/project/data/analysis_data/all_driver_qualifying.csv")

qual2_11 <- read_csv("/cloud/project/data/analysis_data/qualifying2_11th.csv")

pole <- read_csv("/cloud/project/data/analysis_data/poleprediction.csv")

## Test data for max-verstappen perdiction

### check q1sec q3sec column are all numbers 

is.numeric(analysis_data$q3sec)

is.numeric(analysis_data$q1sec)

### check driverId is 830 

all(analysis_data$driverId == 830)

## Test data for prediction of all driver in Q1 Q2 Q3 

### check that q1 q2 q3 are all numbers

all(is.numeric(all_qual$q1sec[!is.na(all_qual$q1sec)]))

all(is.numeric(all_qual$q2sec[!is.na(all_qual$q2sec)]))

all(is.numeric(all_qual$q3sec[!is.na(all_qual$q3sec)]))

### check that there is at lost 10 driver take part in q3. 

results <- all_qual %>%
  group_by(raceId) %>%
  summarize(num_numeric = sum(!is.na(q3sec) & q3sec == as.numeric(q3sec), na.rm = TRUE)) %>%
  mutate(at_most_10_numeric = num_numeric <= 10)

all_meet_criteria <- all(results$at_most_10_numeric)

print(all_meet_criteria)

## Test data for prediction on q2 elimination time 

### check that q1sec q2sec are all numbers

all(is.numeric(qual2_11$q1sec[!is.na(qual2_11$q1sec)]))

all(is.numeric(qual2_11$q2sec[!is.na(qual2_11$q2sec)]))

## Test data for prediction on pole position time

### check that q1sec q2sec q3sec are all numbers

all(is.numeric(pole$q1sec[!is.na(pole$q1sec)]))

all(is.numeric(pole$q2sec[!is.na(pole$q2sec)]))

all(is.numeric(pole$q3sec[!is.na(pole$q3sec)]))

