#### Preamble ####
# Purpose: Build model and use Q1 and Q2 time to predict pole position time. Use qqplot, residual plots, residual distribution to measure the quality of the fit.
# Author: Zhijun Zhong
# Date: March.29, 2024
# Contact: Jerryzz.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: run both 02dataclean and 01downloaddata and check there is such file in analysisdata file.

check_and_install_packages <- function(package_names) {
  for (package_name in package_names) {
    if (!require(package_name, character.only = TRUE)) {
      install.packages(package_name)
      library(package_name, character.only = TRUE)
    }
  }
}

## if not, install package 
packages_needed <- c("ggplot2", "dplyr", "knitr", "janitor", "tidyverse", "lubridate", "readr", "gridExtra", "modelsummary", "broom", "broom.mixed", "stringr", "rstanarm", "jsonlite", "usethis", "devtools", "here", "kableExtra") 
check_and_install_packages(packages_needed)

library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(usethis)
library(readr)
library(devtools)
library(kableExtra)
library(kaggler)
library(ggplot2)
library(knitr)
library(janitor)
library(lubridate)
library(gridExtra)
library(modelsummary)
library(stringr)
library(rstanarm)
library(here)

qual2_11 <- read_csv("/cloud/project/data/analysis_data/qualifying2_11th.csv")

pole <- read_csv("/cloud/project/data/analysis_data/poleprediction.csv")

poletowin <- read_csv("/cloud/project/data/analysis_data/poletowin_rate.csv")

poletowin2 <- read_csv("/cloud/project/data/analysis_data/poletowin2.csv")

medians <- pole %>%
  summarise(
    MedianQ1 = median(q1sec, na.rm = TRUE),
    MedianQ2 = median(q2sec, na.rm = TRUE),
    MedianQ3 = median(q3sec, na.rm = TRUE)
  )

# Calculate differences relative to Q1
medians <- medians %>%
  mutate(
    DiffQ2 = MedianQ2 - MedianQ1,
    DiffQ3 = MedianQ3 - MedianQ1
  )

# Create a dataframe for plotting
data_for_plot <- data.frame(
  Round = c("Q2", "Q3"),
  Difference = c(medians$DiffQ2, medians$DiffQ3)
)

# Plot the differences using histograms
ggplot(data_for_plot, aes(x = Round, y = Difference, fill = Round)) +
  geom_col() +
  scale_fill_manual(values = c("Q2" = "red", "Q3" = "green")) +
  labs(
    title = "Difference in Median Lap Times Relative to Q1",
    x = "Qualifying Round",
    y = "Median Time Difference (seconds)"
  ) +
  theme_minimal()

poletowin <- poletowin %>%
  mutate(names = reorder(names, -percentage))

ggplot(poletowin, aes(x = names, y = percentage)) +
  geom_bar(stat = "identity", fill = "darkgreen") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Driver Name", y = "Percentage", title = "From Pole Position to Win Ratio")

ggplot(poletowin2, aes(x = "", y = Percentage, fill = factor(position.y))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "Qualifying Position", title = "Percentage of Race Wins For Each Qualifying Position") +
  scale_fill_brewer(palette = "Pastel1")

