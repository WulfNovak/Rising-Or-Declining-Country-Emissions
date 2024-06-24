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

librarian::shelf(lme4, randomForest)

# Read in Data
modeling_df <- fread('./datasets/Emissions/modeling_df.rds') %>%
  # restrict to variables of interest
  select(-c('country_code', starts_with('sig_'), contains('slope_'))) %>%
  filter(!declining_emissions_indicator == 'insignificant_trend') %>%
  mutate(declining_emissions_indicator = 
           case_when(declining_emissions_indicator == 'declining' ~ 1,
                     declining_emissions_indicator == 'rising' ~ 0) %>%
           as.factor(),
         year = year %>% as.numeric(),
         country = country %>% as.factor())

# Due to vast NAs with Development indicators, reducing scope greatly
modeling_df_2 <- modeling_df %>%
  select(country, year, coal, oil, gas, declining_emissions_indicator) %>%
  # omit years 2000 and 2001 for Timor-Leste (NAs for Coal and Gas) %>%
  na.omit() %>%
  group_by(country) %>%
  mutate(coal = scale(coal)[,1],
         gas = scale(gas)[,1],
         oil = scale(oil)[,1]) %>%
  ungroup()
  

# Model Preparation -------------------------------------------------------

# predict rising/declining indicator with coal / oil / gas
  # changes in what best predict decline in total emissions?

# Mixed effects linear model

# nlmer
model_1 <- glmer(declining_emissions_indicator ~ 
                   coal + oil + gas + (1 | country),
                 data = modeling_df_2, 
                 family = 'binomial')

# Fixed effects:
#   Estimate     Std. Error z value            Pr(>|z|)    
# (Intercept) -15.0633232186   1.4562534607  -10.34 <0.0000000000000002 ***
# coal         -0.0000004543   0.7728676296    0.00                   1    
# oil           0.0000001968   0.7745200793    0.00                   1    
# gas          -0.0000003011   0.7335881109    0.00                   1    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1








