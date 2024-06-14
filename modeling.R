# ##############################################################################
# Author: Wulf Novak
#
# Project: Development Indicators of Rising 
#          and Declining Country Emissions
#
# Date: 2024-05-24
#
# R version 4.4.0 (2024-04-24 ucrt)
# ------------------------------------------------------------------------------
# Sections:
# - Setup
# - Read in Data
# - Etc
# ##############################################################################

# Setup -------------------------------------------------------------------

source('./R Projects/Emissions-Data-Analysis/Setup.R')

librarian::shelf(lme4)
librarian::shelf(randomForest)

# Read in Data
modeling_df <- fread('./datasets/Emissions/modeling_df.rds') %>%
  # restrict to variables of interest
  select(-c('country_code', starts_with('sig_'), contains('slope_'),
            'total', 'oil', 'coal', 'gas', 'per_capita')) %>%
  filter(!declining_emissions_indicator == 'insignificant_trend') %>%
  mutate(declining_emissions_indicator = 
           case_when(declining_emissions_indicator == 'declining' ~ 1,
                     declining_emissions_indicator == 'rising' ~ 0) %>%
           as.factor(),
         year = year %>% as.numeric(),
         country = country %>% as.factor())

# Model Preparation -------------------------------------------------------

## Assess percentage of missing values of variables by country


# Mixed effects: random = time, country
# norm_tot, per_capita
# Variable selection

# # MICE imputation
# librarian::shelf(mice)
# 
# # currently imputing by country - how to account for trends between countries?
# start <- Sys.time()
# imputed_modeling_df <- modeling_df %>%
#   # impute at the country level
#   group_by(country) %>%
#   mice(m = 100, seed = 43, print = FALSE, #method = rf, #maxit = 10, number of iterations (have enough iterations for mean convergence)
#                             # visitSequence / blocks = country, year?
#                             ) %>%
#   ungroup()
# end <- Sys.time()
# compute_time_default <- abs(start - end)

### Map to impute data at the country level
librarian::shelf(mice)

start <- Sys.time()
test <- modeling_df %>%
  group_by(country) %>%
  group_split()

# if column sum of NAs is greater than x%, remove variable? 

test_v2 <- test[[1]] %>% mice(m = 100, seed = 43, print = FALSE)
 
  # map_dfr(.x = .,
  #         .f = ~mice(data = ., m = 100, seed = 43, print = FALSE)) %>%
  # ungroup()

end <- Sys.time()
compute_time_default <- abs(start - end)

### Consider writing own imputation method



# country by country vip

# # Fit a 1st pass mixed-effects model
# model <- glmer(
#   declining_emissions_indicator ~ . - country - year + (1 | country) + (1 | year), 
#   data = modeling_df,
#   family = binomial)
# 
# summary(model)
# 
# # Check variable importance
# stepAIC(model, direction = "both")











