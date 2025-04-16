rm(list = ls())

library(tidyverse)
library(DBI)
library(readxl)
library(openxlsx)
library(dplyr)


data <- read.csv("/Users/olivia/Dropbox/Mac/Downloads/anonym.csv")

unique(data$Communication_activity_are_you_conducting)
unique(data$What_is_your_age_group). # --> age "14-17 years"

data |>
  count(What_is_your_age_group)