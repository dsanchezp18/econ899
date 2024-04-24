# R Script: DD Quarterly tables
# ECON899 MA Paper 
# SFU Economics
# Daniel Sanchez
# Spring 2024 

# This script implements difference-in-differences models with two-way fixed effects (TWFE) for patents as dependent variables.

# Preliminaries ----------------------------------------------------------------

# Execute required scripts

source("scripts/r/analysis/monthly/dd_patents.R")
source("scripts/r/tables/mappings.R")

# Mapping (only for the treated coefficient)

coef_map_treated <- c(
    treated = "Treatment x Post")


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
                term = c("Explained variable", "Controls"),
                v1 = c("", "None"),
                v2 = c("$\\ln(\\text{Patents}+1)$", "Economic"),
                v3 = c("", "Economic + Additional")
)

# Change the position to above goodness of fit statistics

attr(explained_vars, 'position') <- c(3,4)

# Create the regressions table

dd_twfe_patents_table <-
    modelsummary(dd_twfe_patents,
                 stars = stars,
                 booktabs = T,
                 output = "latex_tabular",
                 escape = F,
                 estimate = "{estimate}{stars}",
                 coef_map = coef_map_treated,
                 add_rows = explained_vars,
                 gof_map = goodness_of_fit_map) %>% 
    row_spec(2, bold = T)

save_kable(dd_twfe_patents_table, "output/tables/dd_twfe_patents_monthly.tex")

# Notes: Standard errors shown in parentheses, clustered at the province and quarter-year level. Fixed effects for province and quarter-year. *** p<0.01, ** p<0.05, * p<0.1.

## DD Patents by section table ---------------------------------------------------

# List of models (only additonal controls)
dd_twfe_patents_section <- list(add_controls_twfe_A,
                                add_controls_twfe_B,
                                add_controls_twfe_C,
                                add_controls_twfe_D,
                                add_controls_twfe_E,
                                add_controls_twfe_F,
                                add_controls_twfe_G,
                                add_controls_twfe_H)

# Explained variables, section codes and names

# explained_vars_section <- tibble(
#                 term = "Patent section (IPC)",
#                 v1 = "A-Human necessities",
#                 v2 = "B-Performing operations; transporting",
#                 v3 = "C-Chemistry; metallurgy",
#                 v4 = "D-Textiles; paper",
#                 v5 = "E-Fixed constructions",
#                 v6 = "F-Mechanical engineering",
#                 v7 = "G-Physics",
#                 v8 = "H-Electricity",
#                 v9 = "Multiple sections"
# )

explained_vars_section <- tibble(
                term = "Patent section (IPC)",
                v1 = "A",
                v2 = "B",
                v3 = "C",
                v4 = "D",
                v5 = "E",
                v6 = "F",
                v7 = "G",
                v8 = "H"
)

# Change the position to above goodness of fit statistics

attr(explained_vars_section, 'position') <- c(3,4)

# Create the regressions table

dd_twfe_patents_section_table <-
    modelsummary(dd_twfe_patents_section,
                 stars = stars,
                 booktabs = T,
                 output = "latex_tabular",
                 escape = F,
                 estimate = "{estimate}{stars}",
                 coef_map = coef_map_treated,
                 gof_map = goodness_of_fit_map,
                 add_rows = explained_vars_section) %>% 
    row_spec(2, bold = T)

save_kable(dd_twfe_patents_section_table, "output/tables/dd_twfe_patents_section_monthly.tex")