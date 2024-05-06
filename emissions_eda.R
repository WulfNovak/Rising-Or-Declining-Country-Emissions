##################################################################
# Author: Wulf Novak
#
# Project: Emissions based - Will decide after EDA
#
# Data: https://www.kaggle.com/datasets/thedevastator/
#       global-fossil-co2-emissions-by-country-2002-2022
#
# Date: 5/3/2024
#
# R version 4.2.2 (2022-10-31 ucrt)
##################################################################

# Setup -------------------------------------------------------------------

### Libraries
library(librarian)
librarian::shelf(tidyverse, data.table, future, furrr)

### Read in Data - Apologies for the absolute path!

# Country level Emissions - primary dataset for analysis
mt_c02 <- fread('C:/Users/WulfN/Python Projects/datasets/Emissions/GCB2022v27_MtCO2_flat.csv')

# Per Capita Emissions
per_capita <- fread('C:/Users/WulfN/Python Projects/datasets/Emissions/GCB2022v27_percapita_flat.csv')

# Sources of data 
sources <- fread('C:/Users/WulfN/Python Projects/datasets/Emissions/GCB2022v27_sources_flat.csv') %>%
  # Replace '[NONE]' entries with NAs
  map_df(., ~case_when(. == "[NONE]" ~ NA, 
                       TRUE ~ .))

# Missing Values Function
  missing_vals <- function(dataframe) {
    
    require(purrr)
    require(dplyr)
    
    nrow <- nrow(dataframe)
    
    # Loop through each column and give stats on key missing value metrics
    results <- tibble(
      variables = colnames(dataframe),
      
      n_obs = nrow,
      
      distinct_obs = dataframe %>% 
        map_dfr(., ~n_distinct(.)) %>% 
        unlist(use.names = FALSE),
      
      qty_missing = dataframe %>%
        map_dfr(., ~case_when(is.na(.) == TRUE ~ 1,
                              is.nan(.) == TRUE ~ 1,
                              TRUE ~ 0)) %>%
        reframe(across(everything(), ~sum(.))) %>%
        unlist(use.names = FALSE),
      
      perc_missing = round(qty_missing * 100 / nrow, 2),
      
      qty_blank = dataframe %>%
        map_dfr(., ~case_when(gsub("\\s+", " ", .) == ' ' ~ 1,
                              TRUE ~ 0)) %>%
        reframe(across(everything(), ~sum(.))) %>%
        unlist(use.names = FALSE),
      
      perc_blank = round(qty_blank * 100 / nrow, 2)
    )
    
    return(results)
  }

# EDA ---------------------------------------------------------------------

# Missing values for dataframes
  missing_vals(mt_c02)
# variables            n_obs distinct_obs qty_missing perc_missing qty_blank perc_blank
# 1 Country            63104          232           0         0            0          0
# 2 ISO 3166-1 alpha-3 63104          226           0         0            0          0
# 3 Year               63104          272           0         0            0          0
# 4 Total              63104        16194         200         0.32         0          0
# 5 Coal               63104        10834       41360        65.5          0          0
# 6 Oil                63104        11735       41387        65.6          0          0
# 7 Gas                63104         6444       41486        65.7          0          0
# 8 Cement             63104         9120       42290        67.0          0          0
# 9 Flaring            63104         3579       41554        65.8          0          0
# 10 Other             63104         1520       61484        97.4          0          0
# 11 Per Capita        63104        17112       44130        69.9          0          0
  
  missing_vals(per_capita)
# variables            n_obs distinct_obs qty_missing perc_missing qty_blank perc_blank
# 1 Country            63104          232           0          0           0          0
# 2 ISO 3166-1 alpha-3 63104          226           0          0           0          0
# 3 Year               63104          272           0          0           0          0
# 4 Total              63104        17054       44132         69.9         0          0
# 5 Coal               63104        10872       45966         72.8         0          0
# 6 Oil                63104        15975       46065         73           0          0
# 7 Gas                63104         6851       46092         73.0         0          0
# 8 Cement             63104         9856       47656         75.5         0          0
# 9 Flaring            63104         3920       46160         73.2         0          0
# 10 Other             63104         1581       61484         97.4         0          0
  
  missing_vals(sources)
# variables            n_obs distinct_obs qty_missing perc_missing qty_blank perc_blank
# 1 Country            63104          232           0          0           0          0
# 2 ISO 3166-1 alpha-3 63104          226           0          0           0          0
# 3 Year               63104          272           0          0           0          0
# 4 Total              63104           25       44636         70.7         0          0
# 5 Coal               63104           27       44631         70.7         0          0
# 6 Oil                63104           24       44860         71.1         0          0
# 7 Gas                63104           25       44887         71.1         0          0
# 8 Cement             63104            4       42018         66.6         0          0
# 9 Flaring            63104           21       44955         71.2         0          0
# 10 Other             63104           18       60996         96.7         0          0
# 11 Per Capita        63104           35       41381         65.6         0          0

# Data from 1750 onwards for some countries  
mt_c02 %>% distinct(Year) %>% summary()  

#         Year     
# Min.   :1750  
# 1st Qu.:1818  
# Median :1886  
# Mean   :1886  
# 3rd Qu.:1953  
# Max.   :2021


# Country Level Summary Statistics
sum_stats_country <- mt_c02 %>%
  mutate( 
    # World level stats
    world_emissions = sum(Total, na.rm = T),
    max_world_emissions = max(Total, na.rm = T),
    world_coal_emissions = sum(Coal, na.rm = T),
    world_oil_emissions = sum(Oil, na.rm = T),
    world_gas_emissions = sum(Gas, na.rm = T),
    world_cement_emissions = sum(Cement, na.rm = T),
    world_other_emissions = sum(Other, na.rm = T),
    world_flaring_emissions = sum(Flaring, na.rm = T)
  ) %>%
  # group_by(Year) %>% # Year level stats
  # mutate(
  #   # number of countries with emissions
  #   n_countries_w_emissions_data = sum(Total > 0, na.rm = T),
  #   max_year_emissions = max(Total, na.rm = T),
  #   year_perc_coal,
  #   year_perc_oil,
  #   year_perc_gas,
  #   year_perc_cement,
  #   year_perc_other
  # )
  # ungroup() %>%
  group_by(Country) %>% # Country level stats
  transmute(
    tot_emissions = sum(Total),
    max_emissions = max(Total),
    # slope emissions?
    years_of_data = sum(Total > 0)
    # Other calculated stats
  )

  
  
### Visualizations

# Total Emissions by Country Over Time


  

# Modeling ----------------------------------------------------------------

# Time Series
# --- Clustering? 


# Results + Visualizations ------------------------------------------------

# This may be completed in the EDA section
# Chorographs! 
  # - leaflet

