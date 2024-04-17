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

qual2_11 <- read_csv("/cloud/project/data/analysis_data/qualifying2_11th.csv")

pole <- read_csv("/cloud/project/data/analysis_data/poleprediction.csv")

recent_driver <- read_csv("/cloud/project/data/analysis_data/poletowin_rate.csv")

position_counts <- read_csv("/cloud/project/data/analysis_data/poletowin2.csv")

## Test data for prediction on q2 elimination time

### check that q1sec q2sec are all numbers

all(is.numeric(qual2_11$q1sec[!is.na(qual2_11$q1sec)]))

all(is.numeric(qual2_11$q2sec[!is.na(qual2_11$q2sec)]))

## Test data for prediction on pole position time

### check that q1sec q2sec q3sec are all numbers

all(is.numeric(pole$q1sec[!is.na(pole$q1sec)]))

all(is.numeric(pole$q2sec[!is.na(pole$q2sec)]))

all(is.numeric(pole$q3sec[!is.na(pole$q3sec)]))

## Test data graph of pole to win rate

### check that percentage are all numbers

is_column_numeric <- is.numeric(recent_driver$percentage)

is_column_numeric

### check that all percentage are smaller than 100 

are_all_numbers_and_less_than_100 <- all(suppressWarnings(!is.na(as.numeric(as.character(recent_driver$percentage)))) &
                                           as.numeric(as.character(recent_driver$percentage)) < 100)
are_all_numbers_and_less_than_100

### Test that names are all strings 

are_all_strings <- all(sapply(recent_driver$names, is.character))

are_all_strings

## Test data the pie graph of each pole how many win 

### check percentage are all numbers 

is_column_numeric_1 <- is.numeric(position_counts$Percentage)

is_column_numeric_1

### check percentage are smaller than 100 

are_all_numbers_and_less_than_100_1 <- all(suppressWarnings(!is.na(as.numeric(as.character(position_counts$Percentage)))) &
                                           as.numeric(as.character(position_counts$Percentage)) < 100)
are_all_numbers_and_less_than_100_1

### check if wins are all numbers 

wins <- all(sapply(position_counts$Wins, is.numeric))

wins

# style code 

style_file("/cloud/project/scripts/03-test_data.R")
