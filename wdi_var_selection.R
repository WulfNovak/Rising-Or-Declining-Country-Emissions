# ##############################################################################
# Author: Wulf Novak
#
# Project: --- tbd
#
# Date: 2024-05-24
#
# R version 4.4.0 (2024-04-24 ucrt)
# ------------------------------------------------------------------------------
# Sections:
# - Setup
# - Variable Selection Based on Completeness
# ##############################################################################

###
# The following script partially investigates the world development indicator
# data. Further analysis occurred in prior modeling and data preparation scripts.
# It was concluded that there was not sufficient data statistically significant
# models, or, a method of imputation that would avoid spurios results. In the 
# interest of not extending the scope of this work, I have opted to discontinue
# working with this data for the time being. Leaving this script here as a vestige
# of prior work.
###

# Goal: Select indicator variables with high global data coverage
#
# Steps to acheive goal
# 1. Assess missing data globally, and by country
#   columns = var, max global missing, max country missing
# 2. Limit to < 25% NAs allowed globally, and x% max per country.
# 3. See what variables fit that criteria.
# 4. Select subset for emissions modeling.
# 5. Transform and impute remaining values by country. 

# Setup -------------------------------------------------------------------

source('./R Projects/Emissions-Data-Analysis/Setup.R')

# Write emissions_dt_2 to datafile
emissions_dt_2 <- fread(file = './datasets/Emissions/emissions_dt_2.rds')

wdi_csv <- fread('./datasets/world_data/WDICSV.csv', header = T) %>%
  filter(`Country Code` %in% emissions_dt_2$country_code) %>%
  clean_names()
### Categorized data
# na_problem <- fread('./R Projects/Emissions-Data-Analysis/Data/Development_Indicators_Categorized.csv')



# Variable Selection Based on Completeness --------------------------------

# Prior iterations of variable selection have failed due to lack of 
# data available across all countries from 2000 forward. Refining list
# of 1492 indicators based on completeness first

perc_missing <- wdi_csv %>%
# omitting 2023 because emissions data is only to year 2022
select(-c(indicator_code, x2023)) %>%
  pivot_longer(cols = x1960:x2022, names_to = "year") %>%
  mutate(year = gsub("x", "", year) %>% as.integer()) %>%
  filter(year >= 2000) %>%
  # perc NAs by indicator
  group_by(country_name, indicator_name) %>%
  mutate(perc_na = round(sum(is.na(value) * 100) / n(), 2)) %>%
  ungroup() %>%
  group_by(indicator_name) %>%
  mutate(global_perc_na = round(mean(perc_na, na.rm = T), 2)) %>%
  ungroup() %>%
  filter(global_perc_na > 25)

# distn of percent missing across all countries
distn_perc_missing <- perc_missing %>% 
  select(-c('global_perc_na', 'year')) %>% 
  distinct(country_name, indicator_name, .keep_all = T) %>%
  group_by(indicator_name) %>%
  reframe(
    min_perc_na = min(perc_na),
    mean_perc_na = mean(perc_na),
    max_perc_na = max(perc_na),
    sd_perc_na = sd(perc_na),
    # vars are not likely normally dist'd, using imprecise method
    upper_bound_perc_na = 1.645 * sd_perc_na + mean_perc_na,
    num_countries_all_na = sum(perc_na == 100)
  ) %>%
  filter(upper_bound_perc_na < 80,
         mean_perc_na <= 50)

# hist of number of countries with 100% NA for indicators
ggplot(distn_perc_missing, aes(x = num_countries_all_na)) + 
  geom_histogram()
  # ** Only 4 indicators have some values for all countries

# There are always countries missing all of the data for a given variable
# impute by clustering on countries with similar values in other variables?
# pool values by year of variable for that cluster

###
selected_wdi_vars <- perc_missing %>% 
  filter(indicator_name %in% distn_perc_missing$indicator_name) %>%
  select(-c('perc_na', 'global_perc_na')) %>%
  pivot_wider(names_from = indicator_name, values_from = value) %>%
  clean_names()

selected_wdi_vars

# For all variables, there exists a country that has no observations.
# In order to produce statistically significant models, considerable imputation
# would need to take place at the country level. To avoid spurious results, 
# will no use this dataset for modeling. 











