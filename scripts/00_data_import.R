library(sotkanet)
library(tidyverse)
library(magrittr)
library(readxl)
library(sjlabelled)

# Data on municipalities with zero out-of-home care cases
municipalities_with_zeros <- read_excel(here("data", "raw", "kunnat joissa ei sijoituksia 1992_2020_valmis.xlsx"))

municipalities_with_zeros  %<>% 
  pivot_longer(!c(Kuntakoodi, Kunta), 
               names_to = "year", 
               values_to = "ohc_n_zero")  %>%
  filter(ohc_n_zero == "0") %>%
  rename(region_code = "Kuntakoodi",
         region.name = "Kunta") %>% 
  mutate(year = as.numeric(year))

# Loading data directly from sotkanet - in two phases: total and sex specific figures

#selecting variables 
wanted_variables <- c(181, 1065, 423, 179)

#importing datasets - first only control variables. These are downloaded separately because they are not sex specific.
#note that absolute value is usually number of people/families. Primary value is usually percentage. Here we take the absolute number because they are more accurate.
dsw.controls <- GetDataSotkanet(indicators = wanted_variables, 
                                genders = c("total"),
                                years = 1990:2021) %>%
  mutate(wanted_value = 
           case_when(indicator == 181 ~ as.numeric(primary.value), 
                     TRUE ~ as.numeric(absolute.value)))  %>%
  pivot_wider(id_cols = c(region, year, region.title.fi,region.category), 
              names_from = c(indicator.title.fi, indicator), 
              values_from = wanted_value)  %>%
  rename(total_chld_n = ends_with("1065"))


#take the sex specific figures 
wanted_variables <- c( 1065, 191, 5495, 5496, 5497, 7, 552:570, 3563, 1066)

#importing datasets 
dsw.main <- GetDataSotkanet(indicators = wanted_variables, 
                            genders = c("total", "female", "male"), 
                            years = 1990:2021) %>%
  mutate(wanted_value = 
           case_when(indicator %in% c(552:570) ~ as.numeric(primary.value), 
                     TRUE ~ as.numeric(absolute.value)))  %>%
  pivot_wider(id_cols = c(region, year, region.title.fi,region.category, gender),
              names_from = c(indicator.title.fi, indicator),
              values_from = wanted_value)

#merging the two datasets - gender specific and general 
dsw <- merge(dsw.controls,
             dsw.main,by=c("region","year"), 
             sort=T, 
             all.y = T, 
             all.x = T)

#We then labeling the data - we take variable names and place them as variable labels 
var.labels <- colnames(dsw) 
dsw <- set_label(dsw, label = var.labels) 

#data wrangling - making more informative variable names 
dsw <- 
  rename(dsw,
         unemp = ends_with("181"), 
         childr_n = ends_with("1065"), 
         childr0018_n = ends_with("1066"), 
         sa_n = ends_with("423"), 
         famil_n = ends_with("179"), 
         ohc_n = ends_with("191"), 
         ohc0006_n = ends_with("5495"), 
         ohc1317_n = ends_with("5496"), 
         ohc0712_n = ends_with("5497"), 
         childr0_n = "0-vuotiaat, lkm 31.12._552", 
         childr1_n = "1-vuotiaat, lkm 31.12._553", 
         childr2_n = "2-vuotiaat, lkm 31.12._554", 
         childr3_n = "3-vuotiaat, lkm 31.12._555", 
         childr4_n = "4-vuotiaat, lkm 31.12._556", 
         childr5_n = "5-vuotiaat, lkm 31.12._557", 
         childr6_n = "6-vuotiaat, lkm 31.12._558", 
         childr7_n = "7-vuotiaat, lkm 31.12._559", 
         childr8_n = "8-vuotiaat, lkm 31.12._560", 
         childr9_n = "9-vuotiaat, lkm 31.12._561", 
         childr10_n = "10-vuotiaat, lkm 31.12._562", 
         childr11_n = "11-vuotiaat, lkm 31.12._563", 
         childr12_n = "12-vuotiaat, lkm 31.12._564", 
         childr13_n = "13-vuotiaat, lkm 31.12._565", 
         childr14_n = "14-vuotiaat, lkm 31.12._566", 
         childr15_n = "15-vuotiaat, lkm 31.12._567", 
         childr16_n = "16-vuotiaat, lkm 31.12._568", 
         childr17_n = "17-vuotiaat, lkm 31.12._569", 
         childr18_n = "18-vuotiaat, lkm 31.12._570", 
         region.name = "region.title.fi.x", 
         region.class = "region.category.x", 
         ohc_0024n = ends_with("3563") ) 

#merging dataset with the zeros
#dsw <- merge(dsw,
#             municipalities_with_zeros,
#             by=c("region.name","year"), 
#             sort=T, 
#             all.y = T, 
#             all.x = T)
dsw <- dsw %>% full_join(municipalities_with_zeros)

#recoding variables when there are no out-of-home cases in the municipality
dsw %<>%  
  mutate_at(vars(c('ohc_n', 'ohc0006_n', 'ohc0712_n', 'ohc1317_n')), 
            .funs = ~case_when(ohc_n_zero == "0" ~ as.numeric(ohc_n_zero), 
                               TRUE ~ .))
# Saving data to rds format
saveRDS(dsw, file = here("data", "raw", "data_full.rds"))
