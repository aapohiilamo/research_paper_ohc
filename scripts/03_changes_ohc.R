# Load necessary libraries
library(tidyverse)
library(magrittr)

# Load the processed data
dsw.d <- readRDS(here("data", "processed", "data_imputed.rds"))

# Calculate increases in OHC from 1992 to 2021

# Calculate increase in OHC for preteenagers (ages 0-12)
koko_maa21 <- filter(dsw.d, region.class == "MAA" & gender == "total" & year == "2021")
koko_maa92 <- filter(dsw.d, region.class == "MAA" & gender == "total" & year == "1992")
increase_preteen <- (koko_maa21$ohc0012 / koko_maa92$ohc0012) * 100
cat("Increase from 1992 to 2021 in OHC in preteenagers: ", increase_preteen, "% \n")

# Calculate increase in OHC for teenagers (ages 13-17)
increase_teen <- (koko_maa21$ohc1317 / koko_maa92$ohc1317) * 100
cat("Increase from 1992 to 2021 in OHC in teenagers: ", increase_teen, "% \n")

# Calculate overall increase in OHC
increase <- (koko_maa21$ohc / koko_maa92$ohc) * 100
cat("Increase from 1992 to 2021 in OHC in all: ", increase, "% \n")

# Calculate increase in OHC for women (gender: female)
koko_maa21 <- filter(dsw.d, region.class == "MAA" & gender == "female" & year == "2021")
koko_maa92 <- filter(dsw.d, region.class == "MAA" & gender == "female" & year == "1992")
increase_female <- (koko_maa21$ohc / koko_maa92$ohc) * 100
cat("Increase from 1992 to 2021 in OHC in women: ", increase_female, "% \n")

# Calculate increase in OHC for men (gender: male)
koko_maa21 <- filter(dsw.d, region.class == "MAA" & gender == "male" & year == "2021")
koko_maa92 <- filter(dsw.d, region.class == "MAA" & gender == "male" & year == "1992")
increase_male <- (koko_maa21$ohc / koko_maa92$ohc) * 100
cat("Increase from 1992 to 2021 in OHC in men: ", increase_male, "% \n")

# Calculate the strength of association between OHC and poverty

# Calculate the strength of association in 1992
koko_maa21 <- filter(dsw.d, region.class == "KUNTA" & gender == "total" & year == "2021")
koko_maa92 <- filter(dsw.d, region.class == "KUNTA" & gender == "total" & year == "1992")
coef_92 <- summary(lm(ohc ~ poverty, data = koko_maa92))$coefficients["poverty", "Estimate"]
cat("The strength of the association in 1992: ", coef_92, "\n")

# Calculate the strength of association in 2021
coef_21 <- summary(lm(ohc ~ poverty, data = koko_maa21))$coefficients["poverty", "Estimate"]
cat("The strength of the association in 2021: ", coef_21, "\n")


#calculate changes by area

changes_area <- filter(dsw.d, region.class == "KUNTA" & gender == "total")

# Filter the dataset for the desired years
library(tidyr)

# Filter the dataset for the desired years
filtered_data <- changes_area %>%
  filter(year %in% c(1992,2021)) %>%
  select(region.name, ohc, year)

# Pivot the data to wide format
pivoted_data <- filtered_data %>%
  pivot_wider(names_from = year, values_from = ohc, names_prefix = "ohc_") %>%
  mutate(change = (100*ohc_2021/ohc_1992)-100) %>%
  select(region.name, change)
# Print the resulting dataset
print(pivoted_data)

