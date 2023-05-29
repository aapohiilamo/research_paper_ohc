# Load required libraries
library(lme4)
library(tidyverse)
library(magrittr)
library(modelr)

# Load the processed data
dsw <- readRDS(here("data", "processed", "data_included.rds"))

# Filter data for municipalities with 'KUNTA' region class and 'total' gender
municipalities_total <- filter(dsw, region.class == "KUNTA" & gender == "total")

# Count the number of unique regions and total observations
num_regions <- length(unique(municipalities_total$region))
num_observations <- nrow(municipalities_total)

cat("Number of unique regions: ", num_regions, "\n")
cat("Number of observations: ", num_observations, "\n")

# Calculate the percentage of missing 'ohc_n' observations
missing_ohc_percentage <- 100 * nrow(filter(municipalities_total, is.na(ohc_n))) / num_observations

cat("Percentage of missing 'ohc_n' observations: ", missing_ohc_percentage, "\n")

# Linear model for imputations - all 'ohc'
linear_model <- lm(ohc_n ~ factor(year) + childr_n + factor(region) +
                     factor(gender) + sa_n + factor(year):sa_n, data = dsw)

# Calculate the R-squared of the linear model
r_squared <- summary(linear_model)$r.squared

cat("R-squared of the linear model: ", r_squared, "\n")

# Add predictions from the linear model to the dataset
dsw <- dsw %>% add_predictions(linear_model)

# Generate imputed values based on the predictions and assign them to new variables
dsw <- dsw %>% ungroup() %>% mutate(
  pred_all = case_when(!is.na(ohc_n) ~ ohc_n,
                       pred > 4 ~ 4,
                       pred < 0 ~ 1,
                       TRUE ~ round(pred, digits = 0))
)

# Linear model for imputations - ages 0-6 'ohc'
liner_model <- lm(ohc0006_n ~ factor(year) + childr_n + pred_all +
                    sa_n + factor(year):sa_n, data = dsw)

# Add predictions from the linear model to the dataset and generate imputed values
dsw <- dsw %>% add_predictions(liner_model, var = "pred2") %>%
  mutate(
    pred_0006 = case_when(pred2 > 4 ~ 4,
                          pred2 < 0 ~ 1,
                          TRUE ~ round(pred2, digits = 0))
  )

# Linear model for imputations - ages 7-12 'ohc'
liner_model <- lm(ohc0712_n ~ factor(year) + childr_n + pred_all +
                    sa_n + factor(year):sa_n, data = dsw)

# Add predictions from the linear model to the dataset and generate imputed values
dsw <- dsw %>% add_predictions(liner_model, var = "pred3") %>%
  mutate(
    pred_0712 = case_when(pred3 > 4 ~ 4,
                          pred3 < 0 ~ 1,
                          TRUE ~ round(pred2, digits = 0))
  )

# Linear model for imputations - ages 13-17 'ohc'
liner_model <- lm(ohc1317_n ~ factor(year) + childr_n + pred_all +
                    sa_n + factor(year):sa_n, data = dsw)

# Add predictions from the linear model to the dataset and generate imputed values
dsw <- dsw %>% add_predictions(liner_model, var = "pred4") %>%
  mutate(
    pred_1317 = case_when(pred4 > 4 ~ 4,
                          pred4 < 0 ~ 1,
                          TRUE ~ round(pred4, digits = 0))
  )

# Replace missing values with imputed values and perform additional data wrangling
dsw <- dsw %>% ungroup() %>% mutate(
  ohc_n = case_when(is.na(ohc_n) ~ pred_all,
                    TRUE ~ ohc_n),
  ohc0006_n = case_when(is.na(ohc0006_n) ~ pred_0006,
                        TRUE ~ ohc0006_n),
  ohc0712_n = case_when(is.na(ohc0712_n) ~ pred_0712,
                        TRUE ~ ohc0712_n),
  ohc1317_n = case_when(is.na(ohc1317_n) ~ pred_1317,
                        TRUE ~ ohc1317_n),
  year = as.integer(year)
) %>%
  mutate(
    poverty = (sa_n / famil_n) * 100,
    low_income = (low_income / childr_n) * 100,
    ohc = (ohc_n / childr0018_n) * 100000,
    childr0012_n = rowSums(across(childr0_n:childr12_n)),
    childr1317_n = rowSums(across(childr13_n:childr18_n)),
    ohc0012 = ((ohc0006_n + ohc0712_n) / childr0012_n) * 100000,
    ohc1317 = (ohc1317_n / childr1317_n) * 100000
  )

# Select key variables for further analysis
dsw.d <- dsw %>% drop_na(poverty, ohc_n) %>% 
  select(poverty, ohc_n, ohc0012, ohc1317, poverty, ohc, unemp, low_income,
         region.class, gender, region, year, region.name, childr0018_n,
         childr0012_n, childr1317_n, total_chld_n, childr_n)

# Save the processed data as an RDS file
saveRDS(dsw.d, file = here("data", "processed", "data_imputed.rds"))

# Create a dataset with age-group specific figures
all <- dsw.d %>% select(ohc, year, region, gender, region.class, poverty, childr0018_n, unemp)
all <- rename(all, ohc_age = "ohc", child_n_age = "childr0018_n")
all$age.group <- "Ages 0 - 17"
all$a.group <- "All"

preteen <- dsw.d %>% select(ohc0012, year, region, gender, region.class, poverty, childr0012_n, unemp)
preteen <- rename(preteen, ohc_age = "ohc0012", child_n_age = "childr0012_n")
preteen$age.group <- "Ages 0 - 12"
preteen$a.group <- "By age group"

teen <- dsw.d %>% select(ohc1317, year, region, gender, region.class, poverty, childr1317_n, unemp)
teen <- rename(teen, ohc_age = "ohc1317", child_n_age = "childr1317_n")
teen$age.group <- "Ages 13 - 17"
teen$a.group <- "By age group"

teens <- rbind(teen, preteen, all)

# Save the processed teens data as an RDS file
saveRDS(teens, file = here("data", "processed", "data_imputed_teens.rds"))
