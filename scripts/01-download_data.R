#### Preamble ####
# Purpose: Downloads and saves the data from https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020
# Author: Zhijun Zhong
# Date: 24 March 2024
# Contact: Jerryzz.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Login to Kaggle with email and click Kaggle on the top left corner, then click your profile photo, then click setting. Finally, click create new token under API category. Put the downloaded folder under the main directory and run the code. (For reproduction purpose)

#### Workspace setup ####

library(tidyverse)
library(httr)
library(jsonlite)
library(usethis)
library(readr)
library(devtools)
library(kaggler)

#### Download data ####

devtools::install_github("ldurazo/kaggler")

kgl_auth(creds_file = "kaggle.json")

response <- kgl_datasets_download_all(owner_dataset = "rohanrao/formula-1-world-championship-1950-2020")

#### Save data ####
download.file(response[["url"]], "data/temp.zip", mode = "wb")
unzip_result <- unzip("data/temp.zip", exdir = "/cloud/project/data/raw_data/", overwrite = TRUE)

# Remove unnecessary files
file.remove("data/temp.zip")
file.remove("kaggle.json")

# style code 

style_file("/cloud/project/scripts/01-download_data.R")
