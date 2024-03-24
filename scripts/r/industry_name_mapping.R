# R Script: Industry name mapping
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares a Canadian NAICS industry name mapping.

# Preliminaries -----------------------------------------------------------

# Load packages

library(readr, warn.conflicts = F)
library(statcanR, warn.conflicts = F)
library(janitor, warn.conflicts = F)
library(tidyr, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(readxl, warn.conflicts = F)
library(lubridate, warn.conflicts = F)

# Industry name mapping -----------------------------------------------------------

## For employment (LFS)

# Load the industry name mapping from the StatCan table

wages_by_industry_table <- 
    read_csv("data/statcan/raw/lfs_wages_industry_prov_monthly.csv", show_col_types = F)

# Extract the industry names

industry_names <- 
    wages_by_industry_table %>% 
    select(north_american_industry_classification_system_naics) %>% 
    distinct()

# Create a tibble with this information

industries_raw <- 
    tibble(north_american_industry_classification_system_naics= industry_names$north_american_industry_classification_system_naics)

# Create simple names 

names <- c(
       "Total",
       "Goods",
       "Agriculture",
       "Natural resources",
       "Utilities",
       "Construction",
       "Manufacturing",
       "Services",
       "Wholesale and retail", 
       "Transportation",
       "Finance",
       "Business",
       "Professional",
       "Education",
       "Healthcare",
       "Media",
       "Food",
       "Other",
       "Public"
)

# Add all columns 

industries_df <-
       industries_raw %>%
       mutate(industry_name = str_extract(north_american_industry_classification_system_naics, "^[^\\[]+") %>% str_trim(),
              naics_code = str_extract(north_american_industry_classification_system_naics, "\\[.*\\]"),
              short_names = names)


# Export -----------------------------------------------------------

# Save the industry name mapping

write_csv(industries_df, "data/other/industry_name_mapping.csv")