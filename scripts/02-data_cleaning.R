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

#### Load Data ####

circuit <- read_csv("/cloud/project/data/raw_data/circuits.csv")
race <- read_csv("/cloud/project/data/raw_data/races.csv")
qualifying <- read_csv("/cloud/project/data/raw_data/qualifying.csv")
lap <- read_csv("/cloud/project/data/raw_data/lap_times.csv")
results <- read_csv("/cloud/project/data/raw_data/results.csv")
drivers <- read_csv("/cloud/project/data/raw_data/drivers.csv")

#### Model Cleaning ####

recent_race <- race[race$year >= 2006 & race$year <= 2023, ]

race_ids <- recent_race$raceId

results <- results %>%
  filter(raceId %in% race_ids)

recent_qualifying <- qualifying[qualifying$raceId %in% race_ids, ]

recent_driver <- data.frame(driverId = unique(recent_qualifying$driverId))

position_counts <- recent_qualifying %>%
  filter(position == 1) %>% 
  group_by(driverId) %>% 
  summarise(count = n())

recent_driver <- recent_driver %>%
  left_join(position_counts, by = "driverId") %>%
  mutate(count = ifelse(is.na(count), 0, count))

qualifying_wins <- recent_qualifying %>%
  filter(position == 1) %>%
  select(driverId, raceId)

race_wins <- results %>%
  filter(position == 1) %>%
  select(driverId, raceId)

winning_both <- inner_join(qualifying_wins, race_wins, by = c("driverId", "raceId"))

dual_victory_counts <- winning_both %>%
  group_by(driverId) %>%
  summarise(dual_victories = n()) %>%
  ungroup()

if(!is.data.frame(recent_driver)) {
  recent_driver <- data.frame(driverId = recent_driver)
}

recent_driver <- recent_driver %>%
  left_join(dual_victory_counts, by = "driverId") %>%
  mutate(dual_victories = ifelse(is.na(dual_victories), 0, dual_victories))

recent_driver <- recent_driver %>%
  filter (dual_victories != 0)

recent_driver <- recent_driver %>%
  mutate(percentage = (dual_victories / count) * 100)

recent_driver <- recent_driver %>%
  left_join(drivers %>% select(driverId, surname), by = "driverId") %>%
  mutate(names = surname) %>%
  select(-surname)

#### Clean for Model of Q2 elimination time ####

all_qualifying <- recent_qualifying %>%
  filter(!is.na(q1) & q1 != "\\N")

all_qualifying <- all_qualifying %>%
  filter(!is.na(q2) & q2 != "\\N")

all_qualifying <- all_qualifying %>%
  mutate(q1sec = {
    time_parts <- strsplit(as.character(q1), ":")
    sapply(time_parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })

all_qualifying <- all_qualifying %>%
  mutate(q2sec = {
    time_parts <- strsplit(as.character(q2), ":")
    sapply(time_parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })

all_qualifying <- all_qualifying %>%
  mutate(q3sec = {
    time_parts <- strsplit(as.character(q3), ":")
    sapply(time_parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })

#### 11th to 11th prediction ####

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

qualifying_11th <- qualifying_11th %>%
  filter(!is.na(q1sec))

qualifying_11th <- qualifying_11th %>%
  group_by(raceId) %>%
  mutate(q1_rank = rank(q1sec, ties.method = "min")) %>%
  ungroup() 

q1_rank_11 <- qualifying_11th %>%
  filter(q1_rank == 11) %>%
  select(raceId, q1sec)

position_11 <- qualifying_11th %>%
  filter(position == 11) %>%
  select(raceId, q2sec)

combined_dataset <- merge(q1_rank_11, position_11, by = "raceId")

combined_dataset$q1_q2_diff <- combined_dataset$q1sec - combined_dataset$q2sec

q1_q2_diff_5 <- quantile(combined_dataset$q1_q2_diff, 0.05, na.rm = TRUE)
q1_q2_diff_95 <- quantile(combined_dataset$q1_q2_diff, 0.95, na.rm = TRUE)

combined_dataset <- combined_dataset[combined_dataset$q1_q2_diff >= q1_q2_diff_5 & combined_dataset$q1_q2_diff <= q1_q2_diff_95, ]

combined_dataset <- na.omit(combined_dataset)

#### clean for predicting pole position ####

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

pole <- pole %>%
  filter(!is.na(q1sec))

pole <- pole %>%
  group_by(raceId) %>%
  mutate(q1_rank = rank(q1sec, ties.method = "min")) %>%
  ungroup() 

pole <- pole %>%
  group_by(raceId) %>%
  mutate(q2_rank = rank(q2sec, ties.method = "min")) %>%
  ungroup() 

q1_rank_1 <- pole %>%
  filter(q1_rank == 1) %>%
  select(raceId, q1sec)

q2_rank_1 <- pole %>%
  filter(q2_rank == 1) %>%
  select(raceId, q2sec)

position_1 <- pole %>%
  filter(position == 1) %>%
  select(raceId, q3sec)

combined <- merge(q1_rank_1, position_1, by = "raceId")

combined <- merge(combined, q2_rank_1, by = "raceId")

combined <- combined %>%
  filter(!is.na(q3sec))

combined$q1_q3_diff <- combined$q1sec - combined$q3sec

combined$q2_q3_diff <- combined$q2sec - combined$q3sec

q1_q3_diff_5 <- quantile(combined$q1_q3_diff, 0.05)
q1_q3_diff_95 <- quantile(combined$q1_q3_diff, 0.95)

combined <- combined[combined$q1_q3_diff >= q1_q3_diff_5 & combined$q1_q3_diff <= q1_q3_diff_95, ]

q2_q3_diff_5 <- quantile(combined$q2_q3_diff, 0.05)
q2_q3_diff_95 <- quantile(combined$q2_q3_diff, 0.95)

combined <- combined[combined$q2_q3_diff >= q2_q3_diff_5 & combined$q2_q3_diff <= q2_q3_diff_95, ]

#### Save data ####
write_csv(pole_positions_with_race_info, "/cloud/project/data/analysis_data/pole_positions_every_year.csv")

write_csv(ver_qualifying, "/cloud/project/data/analysis_data/verstappen_qualifying.csv")

write_csv(all_qualifying, "/cloud/project/data/analysis_data/all_driver_qualifying.csv")

write_csv(qualifying_11th, "/cloud/project/data/analysis_data/qualifying_11th.csv")

write_csv(combined_dataset, "/cloud/project/data/analysis_data/qualifying2_11th.csv")

write_csv(combined, "/cloud/project/data/analysis_data/poleprediction.csv")

write_csv(recent_driver, "/cloud/project/data/analysis_data/poletowin_rate.csv")
