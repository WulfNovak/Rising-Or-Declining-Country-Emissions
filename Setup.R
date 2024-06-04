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
# ##############################################################################

# Setup -------------------------------------------------------------------

### Libraries
library(librarian)
shelf(tidyverse, data.table, future, furrr, 
      plotly, viridis, svglite, janitor)

options(scipen = 999)

# Missing Values Function
missing_vals <- function(dataframe) {
  
  require(purrr)
  require(dplyr)
  
  nrow <- nrow(dataframe)
  
  # Loop through each column and give stats on key missing value metrics
  results <- tibble(
    variables = colnames(dataframe),
    
    n_distinct = dataframe %>% 
      map_dfr(., ~n_distinct(.)) %>% 
      unlist(use.names = FALSE),
    
    qty_na = dataframe %>%
      map_dfr(., ~case_when(is.na(.) == TRUE ~ 1,
                            is.nan(.) == TRUE ~ 1,
                            TRUE ~ 0)) %>%
      reframe(across(everything(), ~sum(.))) %>%
      unlist(use.names = FALSE),
    
    perc_na = round(qty_na * 100 / nrow, 2),
    
    qty_blank = dataframe %>%
      map_dfr(., ~case_when(gsub("\\s+", " ", .) == ' ' ~ 1,
                            TRUE ~ 0)) %>%
      reframe(across(everything(), ~sum(.))) %>%
      unlist(use.names = FALSE),
    
    perc_blank = round(qty_blank * 100 / nrow, 2),
    
    qty_zero = dataframe %>%
      map_dfr(., ~case_when(. == 0 ~ 1,
                            TRUE ~ 0)) %>%
      reframe(across(everything(), ~sum(.))) %>%
      unlist(use.names = FALSE),
    
    perc_zero = round(qty_zero * 100 / nrow, 2)
  )
  
  return(print(results, n = 999))
}
