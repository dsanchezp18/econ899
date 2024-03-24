# R Script: Data Visualization for Explanatory Variables
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script is used to prepare data visualizations of the explanatory variables

# Preliminaries -----------------------------------------------------------

# Load libraries

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(lubridate)

# Define relevant dates

start_date <- as.Date("1980-01-01")

end_date <- as.Date("2021-12-31")

treatment_starts <- as.Date("2016-08-01")

# Load the data -----------------------------------------------------------

# Load the final dataset

df <- readRDS("data/full_dataset.rds")

# Electric power generation (StatCan) -------------------------------------

# See if electric power generation has missing values

df %>%
       summarise(missing_values = sum(is.na(electric_power_generation))) %>%
       print()

# See missing values by year

df %>%
       filter(is.na(electric_power_generation)) %>%
       group_by(year = year(month_year), province_code)%>%
       summarise(missing_values = sum(is.na(electric_power_generation)))  %>% View()

# Time series plot of national electric power generation



df %>%
       filter(month_year %>% between(start_date, end_date)) %>%
       group_by(month_year) %>% 
       summarise(total_generation = sum(electric_power_generation)) %>%