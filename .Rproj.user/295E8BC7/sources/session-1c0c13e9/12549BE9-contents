# Title: The increasing association between child poverty and children in out-of-home care - an ecological longitudinal analysis of Finnish municipalities in the period 1992-2021

# Purpose : This 

# Project number: #007

# Data file: 

# Author: Aapo Hiilamo
# Contact details: aapo.hiilamo at itla.fi

# Date script created: 30.10.2022
# Date script last modified: 31.12.2022

################################################################################

library(quarto)
library(here)
################################################################################


source(here("scripts", "00_data_import.R"))  #internet connection required
source(here("scripts", "01_sample_selection.R"))
source(here("scripts", "02_imputation_variable_coding.R"))
source(here("scripts", "03_changes_ohc.R"))

quarto_render(here("quarto", "04_figures.qmd"), execute_dir = here())
quarto_render(here("quarto", "05_regressions.qmd"), execute_dir = here())

