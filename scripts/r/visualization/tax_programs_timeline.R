# R Script: Tax Credits Programs Timeline 
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script prepares a timeline visualization of tax credits programs. 

# Preliminaries -----------------------------------------------------------

# Load packages

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

# Load data

df <- 
    read_csv("data/other/rd_tax_credits.csv", show_col_types = F)  %>% 
    clean_names()

# Start and end dates to date

start_date <- "1950-01-01"

end_date <- "2022-12-31"

# Prepare data ------------------------------------------------------------

# Visualization ------------------------------------------------------------

# Dots of tax credits programs over time

timeline_chart <-
    df %>%
    mutate(y_coord = 1)  %>%
    ggplot(aes(x = start_year, y = y_coord, color = province_territory, group = province_territory)) +
    geom_point(size = 3, shape = 3) +
    scale_x_continuous(limits = c(1960, 2022), breaks = seq(1960, 2022, 2)) +
    scale_y_continuous(limits = c(0.5, 1.5)) +
    theme_minimal() +
    geom_hline(yintercept = 1, colour = "grey50") +
    labs(title = "Tax Credits Programs Timeline",
         subtitle = "Canada",
         x = "Year",
         y = "") +
    theme(text = element_text(size = 10, family = 'serif'),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line.x = element_line(colour = "black"),
          plot.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1, linetype = "solid"),
          plot.caption = element_text(hjust = 0),
          panel.grid.major.x = element_line(linetype = "dashed"),
          panel.grid.minor.x = element_line(linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(2.5,0.25, 2.5, 0.25, "cm"))

ggsave("figures/rd_tax_credits_timeline.png", 
        plot = timeline_chart, 
        width = 17, 
        height = 10, 
        units = "cm", 
        dpi = 800)        

