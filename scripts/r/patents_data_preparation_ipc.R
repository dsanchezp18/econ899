# R Script: Patents International Patent Classification (IPC) Data Preparation
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares the patents IPC files (as downloaded from IP Horizons) for later processing.

# Preliminaries -----------------------------------------------------------

# Load packages

library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(tidyr)

# Load parameters ---------------------------------------------------------

# Define load parameters for the read_csv function

# Determining column types and selecting which to load, those which are not needed are defined as "skip"

ipc_columns <- c("patent_number" = "i",
                 "ipc_classification_sequence" = "i",
                 "ipc_version_date" = "D",
                 "classification_level" = "c",
                 "classification_status_code" = "c",
                 "classification_status" = "c",
                 "ipc_section_code" = "c",
                 "ipc_section" = "c",
                 "ipc_class_code" = "c",
                 "ipc_class" = "c",
                 "ipc_subclass_code" = "c",
                 "ipc_subclass" = "c",
                 "ipc_main_group_code" = "c",
                 "ipc_group" = "c",
                 "ipc_subgroup_code" = "c",
                 "ipc_subgroup" = "c")

ipc_columns_names <- names(ipc_columns)

# Loading the data --------------------------------------------------------

# Unzip the file 

unzip("data/patents/raw/zip/PT_IPC_classification_1_to_2000000_2023-06-13.zip", 
      exdir = "data/patents/raw")

unzip("data/patents/raw/zip/PT_IPC_classification_2000001_to_4000000_2023-06-13.zip", 
      exdir = "data/patents/raw")

patents_ipc1 <- read_delim("data/patents/raw/PT_IPC_classification_1_to_2000000_2023-06-13.csv",
                            skip = 1,
                            delim = "|",
                            col_types = ipc_columns,
                            col_names = ipc_columns_names,
                            trim_ws = TRUE,
                            locale = locale(encoding = "UTF-8"))

patents_ipc2 <- read_delim("data/patents/raw/PT_IPC_classification_2000001_to_4000000_2023-06-13.csv",
                            skip = 1,
                            delim = "|",
                            col_types = ipc_columns,
                            col_names = ipc_columns_names,
                            trim_ws = TRUE,
                            locale = locale(encoding = "UTF-8"))

patents_ipc_raw <- bind_rows(patents_ipc1, patents_ipc2)

patents_ipc1_test <- read_delim("data/patents/raw/PT_IPC_classification_1_to_2000000_2023-06-13.csv",
                            delim = "|",
                            trim_ws = TRUE,
                            locale = locale(encoding = "UTF-8"))

# Data preparation --------------------------------------------------------

## Patent IPC Sections ---------------------------------------------------

# Extract each patent IPC section to later join to the main data
# Most have only one section, but some have more than one. These are mapped to the "Multiple" category.
# Use the Basic classification status only

# Get IPC sections in long format for all available patents

patent_sections_long <-
      patents_ipc_raw %>% 
      filter(classification_status_code == "B") %>% 
      transmute(patent_number, 
                ipc_section_code, 
                ipc_section = str_to_sentence(ipc_section)) %>% 
      distinct(patent_number, ipc_section_code, .keep_all = T) %>% 
      arrange(patent_number, ipc_section_code)

# Now get the number of sections per patent

patent_sections_count <-
      patent_sections_long %>% 
      group_by(patent_number) %>% 
      summarise(n_sections = n())

# Get a table with the patents who have more than one section

multiple_section_patents <-
      patent_sections_count %>% 
      filter(n_sections > 1) %>% 
      arrange(desc(n_sections)) %>% 
      mutate(ipc_section = "Multiple", ipc_section_code = "Multiple") %>% 
      ungroup()

# Number of patents with multiple sections

multiple_section_patents_num <- nrow(multiple_section_patents)

# Those patents will have an automatic "Multiple" section.

single_section_patents <-
      patent_sections_long %>% 
      anti_join(multiple_section_patents, by = "patent_number") %>% 
      left_join(patent_sections_count, by = "patent_number") 

# Final table with the sections

patent_sections <-
      bind_rows(single_section_patents, multiple_section_patents) %>% 
      arrange(patent_number, ipc_section_code)

## Join to the main data -------------------------------------------------

## Export the data --------------------------------------------------------

# Write this to an rds file

saveRDS(patent_sections, "data/patents/processed/patent_ipc_sections.rds")