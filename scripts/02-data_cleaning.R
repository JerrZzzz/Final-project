#### Preamble ####
# Purpose: Cleans the raw lap time data for Q1 Q2 Q3 in order to fit model to predict Q3 and elimination time
# Author: Zhijun Zhong
# Date: March. 29, 2024
# Contact: Jerryzz.zhong@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(usethis)
library(readr)
library(devtools)
library(kaggler)
library(arrow)

#### Load Data ####

circuit <- read_csv("/cloud/project/data/raw_data/circuits.csv")
race <- read_csv("/cloud/project/data/raw_data/races.csv")
qualifying <- read_csv("/cloud/project/data/raw_data/qualifying.csv")
lap <- read_csv("/cloud/project/data/raw_data/lap_times.csv")
results <- read_csv("/cloud/project/data/raw_data/results.csv")
drivers <- read_csv("/cloud/project/data/raw_data/drivers.csv")

#### Model Cleaning ####

# select year 2006 to 2023
recent_race <- race[race$year >= 2006 & race$year <= 2023, ]

# find raceId between 2006 to 2023 in results
recent_result <- results %>%
  filter(raceId %in% recent_race$raceId)

# filter those achieve win
recent_result <- recent_result %>%
  filter(position == 1)

# find those wins in qualifying
winners_data <- recent_result %>%
  inner_join(qualifying, by = c("raceId", "driverId"))

# select needed columns for the pole to win graph
winners_data <- winners_data %>%
  select(raceId, driverId, position.y)

# calculate the percentage of a position to win
position_counts_1 <- winners_data %>%
  group_by(position.y) %>%
  summarise(Wins = n()) %>%
  mutate(Percentage = Wins / sum(Wins) * 100)

# find all raceId between 2006 to 2023
race_ids <- recent_race$raceId

# find raceId between 2006 to 2023 in results
results <- results %>%
  filter(raceId %in% race_ids)

# find all qualifying between 2006 to 2023
recent_qualifying <- qualifying[qualifying$raceId %in% race_ids, ]

# Initialize the recent_driver Data Frame
recent_driver <- data.frame(driverId = unique(recent_qualifying$driverId))

# Counting Pole Positions:
position_counts <- recent_qualifying %>%
  filter(position == 1) %>%
  group_by(driverId) %>%
  summarise(count = n())

# Merge Pole Position Counts
recent_driver <- recent_driver %>%
  left_join(position_counts, by = "driverId") %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Filter for Qualifying Wins
qualifying_wins <- recent_qualifying %>%
  filter(position == 1) %>%
  select(driverId, raceId)

# Filter for Race Wins 
race_wins <- results %>%
  filter(position == 1) %>%
  select(driverId, raceId)

# Identify Dual Victories (Qualifying and Race Wins)
winning_both <- inner_join(qualifying_wins, race_wins, by = c("driverId", "raceId"))

# Count Dual Victories
dual_victory_counts <- winning_both %>%
  group_by(driverId) %>%
  summarise(dual_victories = n()) %>%
  ungroup()

# delete NA values
if (!is.data.frame(recent_driver)) {
  recent_driver <- data.frame(driverId = recent_driver)
}

# delete driver with no dual victories 
recent_driver <- recent_driver %>%
  left_join(dual_victory_counts, by = "driverId") %>%
  mutate(dual_victories = ifelse(is.na(dual_victories), 0, dual_victories))

# filter them where dual victories are not zero 
recent_driver <- recent_driver %>%
  filter(dual_victories != 0)

# calculate the precentage
recent_driver <- recent_driver %>%
  mutate(percentage = (dual_victories / count) * 100)

# join the dataset with drivers which add driver's last name 
recent_driver <- recent_driver %>%
  left_join(drivers %>% select(driverId, surname), by = "driverId") %>%
  mutate(names = surname) %>%
  select(-surname)

#### Clean for Model of Q2 elimination time ####

# put all q1, q2 into unit of seconds
qualifying_11th <- recent_qualifying %>%
  mutate(q1sec = {
    time_parts <- strsplit(as.character(q1), ":")
    sapply(time_parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })

qualifying_11th <- qualifying_11th %>%
  mutate(q2sec = {
    time_parts <- strsplit(as.character(q2), ":")
    sapply(time_parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })

# delete all na values
qualifying_11th <- qualifying_11th %>%
  filter(!is.na(q1sec))

# figure out the rank of each driver in q1
qualifying_11th <- qualifying_11th %>%
  group_by(raceId) %>%
  mutate(q1_rank = rank(q1sec, ties.method = "min")) %>%
  ungroup()

# filter those who are 11th fastest in q1 
q1_rank_11 <- qualifying_11th %>%
  filter(q1_rank == 11) %>%
  select(raceId, q1sec)

# filter those who are 11th fastest in q2 
position_11 <- qualifying_11th %>%
  filter(position == 11) %>%
  select(raceId, q2sec)

# combine the datasets
combined_dataset <- merge(q1_rank_11, position_11, by = "raceId")

# calculate the difference of q1 and q2 time 
combined_dataset$q1_q2_diff <- combined_dataset$q1sec - combined_dataset$q2sec

# delete outliers for 5% quantile 
q1_q2_diff_5 <- quantile(combined_dataset$q1_q2_diff, 0.05, na.rm = TRUE)
q1_q2_diff_95 <- quantile(combined_dataset$q1_q2_diff, 0.95, na.rm = TRUE)

# combine datasets
combined_dataset <- combined_dataset[combined_dataset$q1_q2_diff >= q1_q2_diff_5 & combined_dataset$q1_q2_diff <= q1_q2_diff_95, ]

# delete all na values 
combined_dataset <- na.omit(combined_dataset)

#### clean for predicting pole position ####

# put q1, q2, q3 time into second form 
pole <- recent_qualifying %>%
  mutate(q1sec = {
    time_parts <- strsplit(as.character(q1), ":")
    sapply(time_parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })

pole <- pole %>%
  mutate(q2sec = {
    time_parts <- strsplit(as.character(q2), ":")
    sapply(time_parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })

pole <- pole %>%
  mutate(q3sec = {
    time_parts <- strsplit(as.character(q3), ":")
    sapply(time_parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })

# delete na values 
pole <- pole %>%
  filter(!is.na(q1sec))

# rank q1 to find the fastest time 
pole <- pole %>%
  group_by(raceId) %>%
  mutate(q1_rank = rank(q1sec, ties.method = "min")) %>%
  ungroup()

# rank q2 to find the fastest time 
pole <- pole %>%
  group_by(raceId) %>%
  mutate(q2_rank = rank(q2sec, ties.method = "min")) %>%
  ungroup()

# find fastest driver in q1
q1_rank_1 <- pole %>%
  filter(q1_rank == 1) %>%
  select(raceId, q1sec)

# find fastest driver in q2
q2_rank_1 <- pole %>%
  filter(q2_rank == 1) %>%
  select(raceId, q2sec)

# find fastest driver in q3
position_1 <- pole %>%
  filter(position == 1) %>%
  select(raceId, q3sec)

# combine the above datasets 
combined <- merge(q1_rank_1, position_1, by = "raceId")

combined <- merge(combined, q2_rank_1, by = "raceId")

# get rid of all na values in the above combined dataset
combined <- combined %>%
  filter(!is.na(q3sec))

# calculate difference for q1 and q3 and Q2 and q3. 
combined$q1_q3_diff <- combined$q1sec - combined$q3sec

combined$q2_q3_diff <- combined$q2sec - combined$q3sec

# omit outliers 5% quantile for q1
q1_q3_diff_5 <- quantile(combined$q1_q3_diff, 0.05)
q1_q3_diff_95 <- quantile(combined$q1_q3_diff, 0.95)

combined <- combined[combined$q1_q3_diff >= q1_q3_diff_5 & combined$q1_q3_diff <= q1_q3_diff_95, ]

# omite outliers 5% quantile for q2
q2_q3_diff_5 <- quantile(combined$q2_q3_diff, 0.05)
q2_q3_diff_95 <- quantile(combined$q2_q3_diff, 0.95)

combined <- combined[combined$q2_q3_diff >= q2_q3_diff_5 & combined$q2_q3_diff <= q2_q3_diff_95, ]

#### Save data ####

write_csv(combined_dataset, "/cloud/project/data/analysis_data/qualifying2_11th.csv")

write_csv(combined, "/cloud/project/data/analysis_data/poleprediction.csv")

write_csv(recent_driver, "/cloud/project/data/analysis_data/poletowin_rate.csv")

write_csv(position_counts_1, "/cloud/project/data/analysis_data/poletowin2.csv")

write_parquet(position_counts, "/cloud/project/data/analysis_data/poletowin2.parquet")

write_parquet(combined_dataset, "/cloud/project/data/analysis_data/qualifying2_11th.parquet")

write_parquet(combined, "/cloud/project/data/analysis_data/poleprediction.parquet")

write_parquet(recent_driver, "/cloud/project/data/analysis_data/poletowin_rate.parquet")

# Style code 

style_file("/cloud/project/scripts/02-data_cleaning.R")
