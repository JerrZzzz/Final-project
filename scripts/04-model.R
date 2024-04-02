#### Preamble ####
# Purpose: Build model and use Q1 and Q2 time to predict pole position time. Use qqplot, residual plots, residual distribution to measure the quality of the fit. 
# Author: Zhijun Zhong
# Date: March.29, 2024
# Contact: Jerryzz.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: run both 02dataclean and 01downloaddata and check there is such file in analysisdata file. 

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(ggplot2)
library(car)
library(httr)
library(jsonlite)
library(usethis)
library(readr)
library(devtools)
library(kaggler)
library(here)
library(bayesplot)

#### Read data ####
analysis_data <- read_csv("/cloud/project/data/analysis_data/verstappen_qualifying.csv")

all_qual <- read_csv("/cloud/project/data/analysis_data/all_driver_qualifying.csv")

qual2_11 <- read_csv("/cloud/project/data/analysis_data/qualifying2_11th.csv")

pole <- read_csv("/cloud/project/data/analysis_data/poleprediction.csv")

#### predict Q2 time ####

### Build model
model2_11th <- lm(q2sec ~ q1sec, data = qual2_11)

### view model
summary(model2_11th)

### plot linear regression
ggplot(qual2_11, aes(x = q1sec, y = q2sec)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Q1 vs Q2 Qualifying Times", x = "Q1 Time (sec)", y = "Q2 Time (sec)") +
  theme_minimal()

pp_check(model2_11th, type = "dens_overlay")

### plot residuals: no pattern = good fit
plot(model2_11th$residuals)
abline(h=0, col='red')

### Check normal distribution
qqnorm(model2_11th$residuals)
qqline(model2_11th$residuals, col = "red")

### find any outlier
influencePlot(model2_11th, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's Distance")

### anova test
anova(model2_11th)

### residual histogram
residuals_q2 <- residuals(model2_11th)

### Build histogram
hist(residuals_q2, breaks=20, freq=FALSE, main="Histogram of Residuals with Normal Curve")

### find mean and sd for normal distribution
mean_residuals_q2 <- mean(residuals_q2)
sd_residuals_q2 <- sd(residuals_q2)

### add curve of the normal distribution
curve(dnorm(x, mean=mean_residuals_q2, sd=sd_residuals_q2), add=TRUE, col="red")

### add labels
xlab("Residuals")
ylab("Density")

### compare to another model ### 

model2_11th_poly <- lm(q2sec ~ poly(q1sec, 2), data = qual2_11)

summary(model2_11th_poly)

modelsummary::modelsummary(
  list(
    "Predicting q2 elimination time linear model" = model2_11th, 
    "Predicting q2 elimination time nonlinear model" = model2_11th_poly
  ),
  q2sec = "mad",
  fmt = 2
)

#### predicting pole position ####

model_pole <- lm(q3sec ~ q1sec + q2sec, data = pole)

summary(model_pole)

pole_long <- gather(pole, key = "Qualifying", value = "Time", q1sec, q2sec)

ggplot(pole_long, aes(x = Time, y = q3sec, color = Qualifying)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = Qualifying), se = FALSE) +
  labs(title = "Qualifying Times vs. Pole Position Times",
       x = "Qualifying Time (sec)", 
       y = "Pole Position Time (sec)") +
  scale_color_manual(values = c("blue", "green")) +
  theme_minimal()

pole$residuals <- residuals(model_pole)

# Plot residuals
ggplot(pole, aes(x = q3sec, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of Linear Model", x = "Predicted Pole Position Time (sec)", y = "Residuals") +
  theme_minimal()

influencePlot(model_pole, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's Distance")

hist(model_pole$residuals, breaks = 30, main = "Distribution of Residuals", xlab = "Residuals", col = "blue")

mean_residuals_pole <- mean(model_pole$residuals)
sd_residuals_pole <- sd(model_pole$residuals)

### add curve of the normal distribution
curve(dnorm(x, mean=mean_residuals_pole, sd=sd_residuals_pole), add=TRUE, col="red")

### qq plot check residual distribution
qqnorm(model_pole$residuals)
qqline(model_pole$residuals, col = "red")

### add labels
xlab("Residuals")
ylab("Density")

#### Save model ####

saveRDS(
  model2_11th,
  file = "models/Q2_model.rds"
)

saveRDS(
  model_pole,
  file = "models/pole_model.rds"
)

saveRDS(
  model2_11th_poly,
  file = "models/Q2non_model.rds"
)


