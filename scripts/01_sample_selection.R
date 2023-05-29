# Load the required libraries
library(tidyverse)
library(magrittr)

# Read the processed data
dsw <- readRDS(here("data", "raw", "data_full.rds"))

# Filter data for municipalities with total gender
municipalities_total <- filter(dsw, region.class == "KUNTA" & gender == "total")

# Count the number of municipalities, observations, children and children in OHC before any deletions
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)
children <- sum(municipalities_total$childr0018_n)
ohc <- sum(municipalities_total$ohc_n, na.rm =TRUE)
cat("Number of municipalities before any deletions: ", before_deletation_regions,"\n")
cat("Number of observations before any deletions: ", observations,"\n")
cat("Number of children before any deletions: ", children,"\n")
cat("Number of children in OHC before any deletions: ", ohc,"\n")

# Filter data for the years 1992-2021
dsw <- filter(dsw, year %in% (1992:2021))

# Update the municipalities_total data after filtering by years
municipalities_total <- filter(dsw, region.class == "KUNTA" & gender == "total")
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)
children <- sum(municipalities_total$childr0018_n)
ohc <- sum(municipalities_total$ohc_n, na.rm =TRUE)
cat("Number of municipalities in 1992-2021: ", before_deletation_regions,"\n")
cat("Number of observations in 1992-2021: ", observations,"\n")
cat("Number of children in 1992-2021: ", children,"\n")
cat("Number of children in OHC in 1992-2021: ", ohc,"\n")

# Exclude municipalities with total_chld_n less than 1000 or not in region.class "KUNTA"
dsw <- filter(dsw, total_chld_n > 1000 | region.class != "KUNTA")
municipalities_total <- filter(dsw, region.class == "KUNTA" & gender == "total")
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)
children <- sum(municipalities_total$childr0018_n)
ohc <- sum(municipalities_total$ohc_n, na.rm =TRUE)
cat("Number of municipalities in 1992-2021 with more than 1000 children: ", before_deletation_regions,"\n")
cat("Number of observations in 1992-2021 with more than 1000 children: ", observations,"\n")
cat("Number of children in 1992-2021 with more than 1000 children: ", children,"\n")
cat("Number of children in OHC in 1992-2021 with more than 1000 children: ", ohc,"\n")

# Exclude rows with missing sa_n values
dsw <- filter(dsw, !is.na(sa_n))
municipalities_total <- filter(dsw, region.class == "KUNTA" & gender == "total")
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)
children <- sum(municipalities_total$childr0018_n)
ohc <- sum(municipalities_total$ohc_n, na.rm =TRUE)
cat("Number of municipalities in 1992-2021 with more than 1000 children & no missing SA data: ", before_deletation_regions,"\n")
cat("Number of observations in 1992-2021 with more than 1000 children & no missing SA data: ", observations,"\n")
cat("Number of children in 1992-2021 with more than 1000 children & no missing SA data: ", children,"\n")
cat("Number of children in OHC in 1992-2021 with more than 1000 children & no missing SA data: ", ohc,"\n")

# Exclude municipalities if all years have missing OHC data
dsw %<>% group_by(region) %>% mutate(count_na = sum(!is.na(ohc_n))) %>% ungroup()
dsw <- filter(dsw, count_na != 0)
municipalities_total <- filter(dsw, region.class == "KUNTA" & gender == "total")
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)
children <- sum(municipalities_total$childr0018_n)
ohc <- sum(municipalities_total$ohc_n, na.rm =TRUE)
cat("Number of municipalities in 1992-2021 with more than 1000 children & no missing SA data & at least one valid OHC data point: ", before_deletation_regions,"\n")
cat("Number of observations in 1992-2021 with more than 1000 children & no missing SA data & see above: ", observations,"\n")
cat("Number of children in 1992-2021 with more than 1000 children & no missing SA data & see above: ", children,"\n")
cat("Number of children in OHC in 1992-2021 with more than 1000 children & no missing SA data & see above: ", ohc,"\n")

# Drop rows with missing values for specific variables and save the processed data
dsw %<>% drop_na(year, region, childr_n, sa_n)
dsw <- filter(dsw, count_na != 0)
saveRDS(dsw, file = here("data", "processed", "data_included.rds"))

