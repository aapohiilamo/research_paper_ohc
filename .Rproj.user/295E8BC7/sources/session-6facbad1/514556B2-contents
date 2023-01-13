library(tidyverse)
library(magrittr)

dsw <- readRDS(here("data", "raw", "data_full.rds"))
municipalities_total <-   
  filter(dsw, region.class == "KUNTA" & gender == "total")

#how many deletations?
before_deletation_regions <- length(unique(municipalities_total$region))
cat("Number of municipalities before any deletations: ", before_deletation_regions,"\n")
length(unique(municipalities_total$region))
observations <-nrow(municipalities_total)
cat("Number of observations before any deletations: ", nrow(municipalities_total),"\n")


dsw <- filter(dsw,  year %in% (1992:2021))

municipalities_total <-   
  filter(dsw, region.class == "KUNTA" & gender == "total")
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)

cat("Number of municipalities in 1992- 2021: ", before_deletation_regions,"\n")
cat("Number of observations in 1992- 2021: ", observations,"\n")


dsw <- filter(dsw, total_chld_n > 1000 |  region.class != "KUNTA")

municipalities_total <-   
  filter(dsw, region.class == "KUNTA" & gender == "total")
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)

cat("Number of municipalities in 1992- 2021 with more than 1000 children : ", before_deletation_regions,"\n")
cat("Number of observations in 1992- 2021 with more than 1000 children: ", observations,"\n")


dsw <- filter(dsw, !is.na(sa_n))
municipalities_total <-   
  filter(dsw, region.class == "KUNTA" & gender == "total")
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)

cat("Number of municipalities in 1992- 2021 with more than 1000 children & no missing SA data: ", before_deletation_regions,"\n")
cat("Number of observations in 1992- 2021 with more than 1000 children & no missing SA data: ", observations,"\n")


#5.  How many after excluding if all years OHC is missing

dsw %<>%                                    # Count NA by group
  group_by(region) %>%
  dplyr::mutate(count_na = sum(!is.na(ohc_n))) %>%
  ungroup()
dsw <- filter(dsw, count_na != 0)
municipalities_total <-   
  filter(dsw, region.class == "KUNTA" & gender == "total")
before_deletation_regions <- length(unique(municipalities_total$region))
observations <- nrow(municipalities_total)

cat("Number of municipalities in 1992- 2021 with more than 1000 children & no missing SA data & at least one valid OHC data point: ", before_deletation_regions,"\n")
cat("Number of observations in 1992- 2021 with more than 1000 children & no missing SA data & see above: ", observations,"\n")

dsw %<>%   
  drop_na(year, region, childr_n,  sa_n)
dsw <- filter(dsw, count_na != 0)


saveRDS(dsw, file = here("data", "processed", "data_included.rds"))
