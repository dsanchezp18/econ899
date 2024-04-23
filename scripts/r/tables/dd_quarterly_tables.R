# R Script: DD Quarterly tables
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements difference-in-differences models with two-way fixed effects (TWFE) for patents as dependent variables.

# Preliminaries ----------------------------------------------------------------

# Execute required scripts

source("scripts/r/analysis/quarterly/dd_patents.R")
source("scripts/r/tables/mappings.R")

# Load required libraries

library(modelsummary, warn.conflicts = FALSE)
library(kableExtra, warn.conflicts = FALSE)

# Tables ------------------------------------------------------------------------

## DD Patents Model ----------------------------------------------------------------

# List of models
dd_twfe_patents <- list(baseline_twfe,
                        def_controls_twfe,
                        add_controls_twfe)

# Create a dataframe with the explained variable names to be added to the model

explained_vars <- tibble(
                term = "Explained variable",
                v1 = "",
                v2 = "$\\ln(\\text{Patents}+1)$",
                v3 = ""
)

# Change the position to the top of the table

attr(explained_vars, 'position') <- 31

# Create the regressions table

dd_twfe_patents_table <-
    modelsummary(dd_twfe_patents,
                 stars = stars,
                 booktabs = T,
                 output = "latex_tabular",
                 escape = F,
                 estimate = "{estimate}{stars}",
                 coef_map = explanatory_variables_map,
                 gof_map = goodness_of_fit_map,
                 add_rows = explained_vars) %>% 
    row_spec(2, bold = T)

save_kable(dd_twfe_patents_table, "output/tables/dd_twfe_patents.tex")

# Notes: Standard errors shown in parentheses, clustered at the province and quarter-year level. Fixed effects for province and quarter-year. *** p<0.01, ** p<0.05, * p<0.1.