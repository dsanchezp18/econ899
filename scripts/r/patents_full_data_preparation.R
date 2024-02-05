# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares the full patents 

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr)

# Load the data -----------------------------------------------------------

# Load all the processed patents data

readRDS("data/patents/processed/patents_main.rds")

readRDS("data/patents/processed/patents_interested_parties.rds")
