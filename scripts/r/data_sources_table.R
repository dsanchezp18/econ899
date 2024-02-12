# R Script: Data Sources Table Preparation
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares a table with the data sources used in the paper. 

# Preliminaries -----------------------------------------------------------

# Load libraries 

library(readxl)
library(dplyr)
library(modelsummary)

# Read data ----------------------------------------------------------------

# Read the XL file with the data sources table data

data_sources <- read_excel("C:\\Users\\user\\OneDrive\\Documentos\\sfu\\ECON899 MA Paper\\AITC\\Variables.xlsx")