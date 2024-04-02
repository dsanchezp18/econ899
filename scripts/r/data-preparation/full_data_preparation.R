# R Script: Full Dataset Preparation
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This dataset prepares the full dataset for the empirical analysis.

# Preliminaries -----------------------------------------------------------

# Load packages

library(dplyr, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(forcats, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(tidyr, warn.conflicts = F)
library(janitor, warn.conflicts = F)

# Define the valid end date for the IP Horizons data (December 2021)

valid_date <- ymd("2021-12-31")

# Define a treatment date (month-year the AITC was passed)

treatment_start_date <- ymd("2016-08-01")
treatment_group <- "AB"

# Load the data -----------------------------------------------------------

## Explained/Dependent variables -----------------------------------------------------------

# Load all the processed patents data (explained variables)

patents_main <- readRDS("data/patents/processed/patents_main.rds") # "Main" data, prepared from the main table downloaded from IP Horizons

patents_interested_parties <- readRDS("data/patents/processed/patents_interested_parties.rds") # "Interested parties" data, prepared from the interested parties tables downloaded from IP Horizons

patents_inventors <- readRDS("data/patents/processed/patents_inventors.rds") # Only inventors data, prepared from the interested parties tables downloaded from IP Horizons

patents_owners <- readRDS("data/patents/processed/patents_owners.rds") # Only owners data, prepared from the interested parties tables downloaded from IP Horizons

patents_applicants <- readRDS("data/patents/processed/patents_applicants.rds") # Only applicants data, prepared from the interested parties tables downloaded from IP Horizons

patent_province_mapping <- readRDS("data/patents/processed/patent_province_mapping.rds") # Mapping of patents to provinces based on % of interested parties

patents_ipc_sections <- readRDS("data/patents/processed/patent_ipc_sections.rds") # IPC sections data, prepared from the IPC classification tables downloaded from IP Horizons

## Explanatory variables/regressors -----------------------------------------------------------

# Load explanatory variables panel dataset

explanatory_province_month_panel_df <- read_csv("data/explanatory_vars_province_month_panel.csv", show_col_types = F)

## Other data --------------------------------------------------------------------------------------

# Load province codes and names

province_codes <- read_csv("data/other/province_codebook.csv", show_col_types = F)

# Patent data --------------------------------------------------------------------------------------

# Get the number of patents per province and month based on my mapping of patents to provinces
# First do them based on filing date, I will later do them based on grant date

patents_per_province_month_filing <- 
       patents_main %>%
       left_join(patent_province_mapping, by = "patent_number") %>% 
       group_by(province_code_clean, filing_month_year) %>% 
       summarise(patents_filed = n(),
                 foreign_parties = sum(foreign_patents)) %>% 
       ungroup() %>% 
       arrange(province_code_clean, desc(filing_month_year)) 

# Check for duplicates

patents_per_province_month_filing %>% 
       group_by(province_code_clean, filing_month_year) %>% 
       summarise(n = n()) %>% 
       filter(n > 1) 

# Get the number of patents per province and month based on grant date

patents_per_province_month_grant <- 
       patents_main %>% 
       left_join(patent_province_mapping, by = "patent_number") %>% 
       group_by(province_code_clean, grant_month_year) %>% 
       summarise(patents_granted = n()) %>% 
       ungroup() %>% 
       arrange(province_code_clean, desc(grant_month_year))

# Create a panel of provinces and months for the main patent data with expand_grid

months <- seq.Date(from = min(patents_main$filing_month_year, na.rm = T), to = valid_date, by = "month")

provinces <- province_codes$province_code

patents_per_province_only <-
       expand_grid(province_code = provinces, month_year = months) %>% 
       left_join(patents_per_province_month_filing, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>% 
       left_join(patents_per_province_month_grant, by = c("province_code" = "province_code_clean", "month_year" = "grant_month_year")) %>% 
       replace_na(list(patents_filed = 0, patents_granted = 0, foreign_parties = 0)) # Fills missing values with 0, since no patents were filed or granted in those months

# Check for duplicates

patents_per_province_only %>% 
       group_by(province_code, month_year) %>% 
       summarise(n = n()) %>% 
       filter(n > 1)

# Adding IPC sections --------------------------------------------------------------------------------------

# Add IPC sections to the main dataset, based on the patent number
# First, get the number of patents per province and month based on the mapping of patents to provinces
# I get new columns for each IPC section
# Also input 0 for the missing values, since no patents were filed in those sections

patents_per_province_month_section <- 
       patents_main %>%
       left_join(patent_province_mapping, by = "patent_number") %>% 
       left_join(patents_ipc_sections, by = "patent_number") %>%
       select(province_code_clean, ipc_section_code, filing_month_year) %>%
       group_by(province_code_clean, filing_month_year, ipc_section_code) %>%
       summarise(n = n()) %>%
       ungroup() %>% 
       filter(!is.na(ipc_section_code), !is.na(province_code_clean)) %>% 
       pivot_wider(names_from = ipc_section_code,
                   names_prefix = "patents_", 
                   values_from = n,
                   values_fill = 0)

# Join the IPC sections data to the main dataset

patents_per_province <-
       patents_per_province_only %>%
       left_join(patents_per_province_month_section, 
                 by = c("province_code" = "province_code_clean", 
                        "month_year" = "filing_month_year")) %>% 
       replace_na(list(patents_A = 0, patents_B = 0, patents_C = 0, patents_D = 0, patents_E = 0, 
                       patents_F = 0, patents_G = 0, patents_H = 0, patents_Multiple = 0)) # Fills missing values with 0, since no patents were filed in those sections

# Check for duplicates

patents_per_province %>% 
       group_by(province_code, month_year) %>% 
       summarise(n = n()) %>% 
       filter(n > 1)

# Interested parties  --------------------------------------------------------------------------------------

## Adding filing dates to party information ---------------------------------------------------------------

# Add the dates of filing date of the application to the interested parties data from the main dataset. 
# This means joining the main data with the interested parties data through the patent number without having to do strange mappings of patents to provinces. 

interested_parties_with_dates <-  
       patents_interested_parties %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

# Do the same thing for the inventors data

inventors_with_dates <-  
       patents_inventors %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

# Do the same thing for the owners data

owners_with_dates <-  
       patents_owners %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

# Do the same thing for the applicants data

applicants_with_dates <-  
       patents_applicants %>%
       left_join(patents_main %>% select(patent_number, filing_month_year), 
                 by = "patent_number")

## Province-month panel data --------------------------------------------------------------------------------------

# Count the number of interested parties per province and month. Filter out non-Canadian provinces.

interested_parties_province_month <- 
       interested_parties_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(interested_parties = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year)) 

# Count the number of inventors per province and month. Filter out non-Canadian provinces.

inventors_province_month <-
       inventors_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(inventors = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year))

# Count the number of owners per province and month. Filter out non-Canadian provinces.

owners_province_month <-
       owners_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(owners = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year))

# Count the number of applicants per province and month. Filter out non-Canadian provinces.

applicants_province_month <-
       applicants_with_dates %>%
       filter(country_mapped_to_province == 'Canada') %>%
       group_by(province_code_clean, filing_month_year) %>%
       summarise(applicants = n()) %>%
       ungroup()  %>% 
       arrange(province_code_clean, desc(filing_month_year))

# Full dataset preparation --------------------------------------------------------------------------------------

# Finalize the dataset by merging all explained and explanatory variables
# Also, define treatment groups and periods based on the treatment date
# Further, filter the dependent whole dataset to before the valid period for IP Horizons data (before december 2021)
# The periods will be defined as a centered time variable: the number of months since the treatment date.
# t = 0 is the treatment date, t < 0 is the pre-treatment period, and t > 0 is the post-treatment period.
# Also create any transformations of variables required for the analysis
# I also redefine the reference level of the factors to be the control group and the pre-treatment period

# Calculate the log and log + 1 of the number of patents

ln_patents_df <- 
       patents_per_province %>% 
       mutate_at(vars(starts_with("patents_")), ~log(.)) %>% 
       rename_with(~paste0("ln_", .), starts_with("patents_")) %>% 
       select(-foreign_parties)

ln1_patents_df <-
       patents_per_province %>%
       mutate_at(vars(starts_with("patents_")), ~log(. + 1))  %>% 
       rename_with(~paste0("ln1", .), starts_with("patents_")) %>% 
       select(-foreign_parties)

# Dataframe with interested parties and inventors

parties_df <-
       interested_parties_province_month %>%
       left_join(inventors_province_month, by = c("province_code_clean", "filing_month_year")) %>%
       left_join(owners_province_month, by = c("province_code_clean", "filing_month_year")) %>%
       left_join(applicants_province_month, by = c("province_code_clean", "filing_month_year")) 

# Log of all parties variables

ln_parties_df <- 
       parties_df %>% 
       mutate_if(is.integer, ~log(.)) %>% 
       rename(ln_interested_parties = interested_parties,
              ln_inventors = inventors,
              ln_owners = owners,
              ln_applicants = applicants)

# Log of all parties variables + 1

ln1_parties_df <- 
       parties_df %>% 
       mutate_if(is.integer, ~log(. + 1)) %>% 
       rename(ln1_interested_parties = interested_parties,
              ln1_inventors = inventors,
              ln1_owners = owners,
              ln1_applicants = applicants)

# Final dataset

df <- 
       patents_per_province %>% 
       left_join(ln_patents_df, by = c("province_code", "month_year")) %>%
       left_join(ln1_patents_df, by = c("province_code", "month_year")) %>%
       left_join(parties_df, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>%
       left_join(ln_parties_df, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>%
       left_join(ln1_parties_df, by = c("province_code" = "province_code_clean", "month_year" = "filing_month_year")) %>% 
       left_join(explanatory_province_month_panel_df, by = c("province_code", "month_year")) %>%
       mutate(province_code = as_factor(province_code),
              periods = interval(treatment_start_date, month_year)/months(1),
              treatment = if_else(province_code == treatment_group, "Treatment", "Control") %>% as.factor() %>% relevel("Control"),
              post = if_else(month_year >= treatment_start_date , "Post", "Pre") %>% as.factor() %>% relevel("Pre"),
              emp_patenting_ind = emp_manufacturing + emp_wholesale_and_retail + emp_media + emp_professional + emp_healthcare,
              wages_paid_patenting_ind = wages_paid_manufacturing + wages_paid_wholesale_and_retail + wages_paid_media + wages_paid_professional + wages_paid_healthcare) %>%
       arrange(province_code, month_year)

# Check for duplicates

df %>% 
       group_by(province_code, month_year) %>% 
       summarise(n = n()) %>% 
       filter(n > 1)

# Check where does ln_patents not NA

df %>% 
       select(starts_with("ln_patents_")) %>% 
       mutate_all(~is.na(.)) %>% 
       summarise_all(sum)

# Count number of values (either NA or non NA) for each variable

df %>% nrow()

# Export the data --------------------------------------------------------------------------------------

# Save the data to an RDS file

saveRDS(interested_parties_province_month, "data/patents/processed/interested_parties_province_month.rds")

# Save the final dataset to an RDS file

saveRDS(df, "data/full_dataset.rds")

# Save the final dataset to a CSV file, for other software

write_csv(df, "data/full_dataset.csv")