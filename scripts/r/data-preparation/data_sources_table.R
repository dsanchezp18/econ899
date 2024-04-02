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
library(kableExtra)
library(tinytable) # Install using remotes::install_github("vincentarelbundock/tinytable")
library(janitor)

# Read data ----------------------------------------------------------------

# Read the XL file with the data sources table data

data_sources <- 
    read_excel("C:\\Users\\user\\OneDrive\\Documentos\\sfu\\ECON899 MA Paper\\AITC\\Variables.xlsx",
            sheet = "Variables (MA Paper)")  %>% 
    clean_names()
                

# Prepare the data sources table -------------------------------------------

# Select the relevant columns

data_sources_table_df <- 
    data_sources %>% 
    select(-citation) %>%
    rename("Source" = source,
           "Variable" = variable,
           "Note" = note)
    
# Preliminary presentation of the data sources table with datasummary 

tt(data_sources_table_df)