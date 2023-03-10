library(tidyverse)
library(magrittr)

dsw.d <- readRDS(here("data", "processed", "data_imputed.rds"))
municipalities_total <-  
  filter(dsw.d, region.class == "KUNTA" & gender == "total")

koko_maa21 <-   filter(dsw.d, region.class == "MAA" & gender == "total" & year== "2021")
koko_maa92 <-   filter(dsw.d, region.class == "MAA" & gender == "total" & year== "1992")
increase_preteen<-(koko_maa21$ohc0012/koko_maa92$ohc0012)*100
cat("Increase from 1992 to 2021 in OHC in preteenagers: ", increase_preteen,"% \n")

increase_teen<-(koko_maa21$ohc1317/koko_maa92$ohc1317)*100
cat("Increase from 1992 to 2021 in OHC in teenagers: ", increase_teen,"% \n")

increase<-(koko_maa21$ohc/koko_maa92$ohc)*100
cat("Increase from 1992 to 2021 in OHC in all: ", increase,"% \n")

koko_maa21 <-   filter(dsw.d, region.class == "MAA" & gender == "female" & year== "2021")
koko_maa92 <-   filter(dsw.d, region.class == "MAA" & gender == "female" & year== "1992")
increase_female<-(koko_maa21$ohc/koko_maa92$ohc)*100
cat("Increase from 1992 to 2021 in OHC in women: ", increase_female,"% \n")

koko_maa21 <-   filter(dsw.d, region.class == "MAA" & gender == "male" & year== "2021")
koko_maa92 <-   filter(dsw.d, region.class == "MAA" & gender == "male" & year== "1992")
increase_male<-(koko_maa21$ohc/koko_maa92$ohc)*100
cat("Increase from 1992 to 2021 in OHC in men: ", increase_male,"% \n")

koko_maa21 <-   filter(dsw.d, region.class == "KUNTA" & gender == "total" & year== "2021")
koko_maa92 <-   filter(dsw.d, region.class == "KUNTA" & gender == "total" & year== "1992")
coef_92<-summary(lm(ohc*10 ~ poverty, data = koko_maa92))$coefficients["poverty", "Estimate"]
cat("The strength of the association in 1992: ", coef_92,"\n")

coef_21<-summary(lm(ohc*10 ~ poverty, data = koko_maa21))$coefficients["poverty", "Estimate"]
cat("The strength of the association in 2021: ", coef_21,"\n")
