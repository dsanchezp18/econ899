# R Script: Patents Data Dictionary Preparation
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script creates a joint flat data dictionary for the patents data.

# Preliminaries -----------------------------------------------------------

# Load packages

library(readxl)
library(dplyr)
library(openxlsx)
library(janitor)

# Prepare the data dictionary -----------------------------------------------------------

# Load all the sheets in the downloaded data dictionary from the IP Horizons website

# Get the names of all sheets in the Excel file

sheet_names <- excel_sheets("data/patents/metadata/PT_Data Dictionary.xlsx")

# Use lapply to read each sheet into a separate data frame
list_of_sheets <- lapply(sheet_names, function(sheet) {
        read_excel("data/patents/metadata/PT_Data Dictionary.xlsx", sheet = sheet, skip = 1)
})

# Assign each data frame to a separate object in the global environment
for(i in seq_along(list_of_sheets)) {
    assign(sheet_names[i], list_of_sheets[[i]])
}

# Use lapply to read each sheet into a separate data frame and add a new column 'sheet'
list_of_sheets <- lapply(sheet_names, function(sheet) {
        df <- readxl::read_excel("data/patents/metadata/PT_Data Dictionary.xlsx", sheet = sheet, skip = 1)
        df$sheet <- sheet
        df
})

# Assign each data frame to a separate object in the global environment
for(i in seq_along(list_of_sheets)) {
    assign(sheet_names[i], list_of_sheets[[i]])
}

# Bind all of these rows together into a single data frame

patents_data_dictionary <-
    PT_Main %>%
    bind_rows(PT_Priority_Claim)  %>% 
    bind_rows(PT_Interested_Party) %>%
    bind_rows(PT_Abstract) %>%
    bind_rows(PT_Disclosure) %>%
    bind_rows(PT_Claim) %>%
    bind_rows(PT_IPC_Classification)  %>% 
    clean_names() %>%
    select(sheet, variable_name, variable_type, variable_description)

# Save the data dictionary as an Excel file

write.xlsx(patents_data_dictionary, "data/patents/metadata/patents_data_dictionary_flat.xlsx", rowNames = FALSE)
    
