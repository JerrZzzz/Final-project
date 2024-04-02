#### Preamble ####
# Purpose: It simulate using seed 6342 to simulate the lap time for F1 driver in q1 q2 and q3. 
# Author: Zhijun Zhong
# Date: 29 March 2024 
# Contact: Jerryzz.zhong@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
# [...UPDATE THIS...]

#### Simulate data ####

set.seed(6342)

# Initialize the number of races and drivers
num_races <- 100
num_drivers <- 20

# Initialize data frame to store simulated times
qualifying_results <- data.frame(
  race_id = rep(1:num_races, each = num_drivers),
  driver_id = rep(1:num_drivers, num_races),
  q1_time = NA,
  q2_time = NA,
  q3_time = NA
)

# Simulate qualifying times for Q1, Q2, Q3
for (race in 1:num_races) {
  # Simulate Q1 times
  q1_times <- round(runif(num_drivers, min = 75, max = 85), 3) # hypothetical lap times in seconds
  qualifying_results$q1_time[qualifying_results$race_id == race] <- q1_times
  
  # Determine top 15 for Q2
  q2_drivers <- head(order(q1_times), 15)
  
  # Simulate Q2 times, improved from Q1
  q2_improvement <- runif(15, min = 0.1, max = 0.5) # drivers improve by 0.1 to 0.5 seconds
  q2_times <- q1_times[q2_drivers] - q2_improvement
  qualifying_results$q2_time[qualifying_results$race_id == race & qualifying_results$driver_id %in% q2_drivers] <- round(q2_times, 3)
  
  # Determine top 10 for Q3
  q3_drivers <- head(order(q2_times), 10)
  
  # Simulate Q3 times, improved from Q2
  q3_improvement <- runif(10, min = 0.1, max = 0.3) # drivers improve by 0.1 to 0.3 seconds
  q3_times <- q2_times[q3_drivers] - q3_improvement
  qualifying_results$q3_time[qualifying_results$race_id == race & qualifying_results$driver_id %in% q2_drivers[q3_drivers]] <- round(q3_times, 3)
}



