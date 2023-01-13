library(lme4)
library(tidyverse)
library(magrittr)
library(modelr)

rm(list=ls())
load(here("data", "processed", "data_included.RData"))

municipalities_total <-   
  filter(dsw, region.class == "KUNTA" & gender == "total")
length(unique(municipalities_total$region))
nrow(municipalities_total)

#What share of observations are ohc missing (ie will be imputed)
100*nrow(filter(municipalities_total, is.na(ohc_n)))/nrow(municipalities_total)

# linear model for imputations - all ohc
liner_model<-lm(ohc_n~ factor(year) + 
                  childr_n + 
                  factor(region) + 
                  factor(gender) +
                  sa_n +
                  factor(year):sa_n, 
                data = dsw)
summary(liner_model)$r.squared

dsw <- dsw %>% 
  add_predictions(liner_model)


dsw <- dsw %>%  
  ungroup()  %>% 
  mutate(pred_all = case_when(!is.na(ohc_n) ~ ohc_n, pred>4 ~ 4, pred<0 ~ 1, TRUE ~ round(pred, digits = 0) ) )

# linear model for imputations - 0 -6 ages ohc - note that here the previous prediction is used as predictor

liner_model<-lm(ohc0006_n~ factor(year) + 
                  childr_n + 
                  pred_all + 
                  sa_n +
                  factor(year):sa_n, 
                data = dsw)

dsw %<>% 
  add_predictions(liner_model, var = "pred2") %>% 
  mutate(pred_0006 = case_when(pred2>4 ~ 4, 
                               pred2<0 ~ 1, 
                               TRUE ~ round(pred2, digits = 0) ) )

# linear model for imputations - 7- 12 ages ohc 

liner_model<-lm(ohc0712_n~ factor(year) + 
                  childr_n + 
                  pred_all +  
                  sa_n +
                  factor(year):sa_n, 
                data = dsw)

dsw %<>% 
  add_predictions(liner_model, var = "pred3") %>%
  ungroup()  %>% 
  mutate(pred_0712 = case_when(pred3>4 ~ 4, 
                               pred3<0 ~ 1, 
                               TRUE ~ round(pred2, digits = 0) ) )

# linear model for imputations - 13-17 ages ohc 

liner_model<-lm(ohc1317_n~ factor(year) + 
                  childr_n + 
                  pred_all +  
                  sa_n +
                  factor(year):sa_n, 
                data = dsw)

dsw %<>% 
  add_predictions(liner_model, var = "pred4") %>%
  ungroup()  %>% 
  mutate(pred_1317 = case_when(pred4>4 ~ 4, 
                               pred4<0 ~ 1, 
                               TRUE ~ round(pred4, digits = 0) ) )

# putting the imputations in place and some data wrangling
dsw %<>%  ungroup()  %>% mutate(
  ohc_n = case_when(is.na(ohc_n) ~ pred_all, TRUE ~ ohc_n ),
  ohc0006_n = case_when(is.na(ohc0006_n)  ~ pred_0006, TRUE ~ ohc0006_n),
  ohc0712_n = case_when(is.na(ohc0712_n)  ~ pred_0712, TRUE ~ ohc0712_n),
  ohc1317_n = case_when(is.na(ohc1317_n)  ~ pred_1317, TRUE ~ ohc1317_n),
  year = as.integer(year) ) %>%
  mutate(               poverty = ( sa_n / famil_n ) * 100,
                        ohc = (ohc_n / childr0018_n) * 1000,
                        childr0012_n = rowSums(across(childr0_n:childr12_n)),
                        childr1317_n = rowSums(across(childr13_n:childr18_n)))    %>%
  mutate(   ohc0012 = ( ( ohc0006_n + ohc0712_n )  /  childr0012_n ) * 1000,
            ohc1317 = ( ohc1317_n / childr1317_n ) * 1000 )
#childr0012_n = rowSums(.[20:32]),
#    childr1317_n = rowSums(.[33:38]))  


dsw.d <- dsw %>% 
  drop_na(poverty, ohc_n)

# selecting key variables

dsw.d %<>% select(poverty, ohc_n, ohc0012, ohc1317, poverty, ohc, unemp,
                  region.class,gender, region, year, region.name, childr0018_n, childr0012_n, childr1317_n, total_chld_n, childr_n )                   

municipalities_total <-  
  filter(dsw.d, region.class == "MAA" & gender == "total")


municipalities_total <-  
  filter(dsw.d, region.class == "KUNTA" & gender == "total")

length(unique(municipalities_total$region))
save(dsw.d, file = here("data", "processed", "data_imputed.RData"))


#creating different dataset with age_group specific figures
all <- c("ohc", "year", "region","gender", "region.class", "poverty" , "childr0018_n", "unemp")
all <- dsw.d[all]

all <- rename(all,  
              ohc_age = "ohc",
              child_n_age = "childr0018_n")

all$age.group <- "Ages 0 - 17"       
all$a.group <- "All"  

preteen <- c("ohc0012", "year", "region","gender", "region.class", "poverty" , "childr0012_n", "unemp")
preteen <- dsw.d[preteen]

preteen <- rename(preteen,  
                  ohc_age = "ohc0012",
                  child_n_age = "childr0012_n")

preteen$age.group <- "Ages 0 - 12"          
preteen$a.group <- "By age group"  

teen <- c("ohc1317", "year", "region" , "gender", "region.class", "poverty" , "childr1317_n", "unemp")

teen <- dsw.d[teen]

teen <- rename(teen,  ohc_age = "ohc1317",
               child_n_age = "childr1317_n")

teen$age.group <- "Ages 13 - 17"              
teen$a.group <- "By age group"  
teens <-  rbind(teen,preteen, all)

save(teens, file = here("data", "processed", "data_imputed_teens.RData"))