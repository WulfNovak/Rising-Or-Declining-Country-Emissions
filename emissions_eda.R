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
librarian::shelf(tidyverse, data.table, future, furrr, plotly, viridis, svglite)

### Read in Data - Apologies for the absolute path!

# Country level Emissions - primary dataset for analysis
mt_c02 <- fread('C:/Users/WulfN/Python Projects/datasets/Emissions/GCB2022v27_MtCO2_flat.csv') %>%
  rename(Country_Code = `ISO 3166-1 alpha-3`)

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

# Any 'Country' level stats to not consider? 
mt_c02 %>% distinct(Country) %>% View()
# International Transport, Global, Kuwait Oil Fires.

# Adjust Japan emissions to include Ryukyu Islands emissions
# for the 1952-1972 time period they were reported separately.

japan_adj <- mt_c02 %>%
  filter(Country %in% c('Ryukyu Islands', 'Japan')) %>%
  mutate(Country = ifelse(Country == 'Ryukyu Islands', 'Japan', Country)) %>%
  group_by(Country, Year) %>%
  reframe(Country, 
          Country_Code = 'JPN',
          Year,
          Total = sum(Total, na.rm = T), 
          Coal = sum(Coal, na.rm = T),
          Oil = sum(Oil, na.rm = T),
          Gas = sum(Gas, na.rm = T),
          Cement = sum(Cement, na.rm = T),
          Flaring = sum(Flaring, na.rm = T),
          Other = sum(Other, na.rm = T),
          `Per Capita`
         )

# Update Country level emissions with Japan adjustment and remove non-countries
mt_c02_adj <- mt_c02 %>%
  filter(!Country %in% c('International Transport', 'Global', 'Kuwaiti Oil Fires', 
                         'Ryukyu Islands', 'Japan')
         ) %>%
  bind_rows(japan_adj) %>% 
  filter(!Total == 0) # Remove 0 Total emission observations

# Some Countries only have pockets of emissions data (Ex: Antarctica 1998-2007)

# Different in number of distinct country obs and country codes - why?
empty_country_code <- mt_c02_adj %>% filter(Country_Code == '') 
empty_country_code %>% distinct(Country)
#                     Country
# 1: French Equatorial Africa
# 2:       French West Africa
# 3:          Leeward Islands
# 4:  Pacific Islands (Palau)
# *** All are island nations
# * Country Code is not a needed feature

# Visualizations ----------------------------------------------------------

## Df for plotting Total Emissions by Country Over Time
plot_df <- mt_c02_adj %>%
  group_by(Country) %>%
  mutate(tot_emissions = sum(Total, na.rm = T)) %>%
  ungroup() %>%
  transmute(
    Country,
    Year, 
    Total, 
    tot_emissions,
    # Calculate and Create variables for Quartiles
    upper_3rd = quantile(tot_emissions, prob = .75, na.rm = T) %>% unname(),
    upper_2nd = quantile(tot_emissions, prob = .5, na.rm = T) %>% unname(),
    upper_1st = quantile(tot_emissions, prob = .25, na.rm = T) %>% unname(),
    country_emissions_quartile = case_when(
      tot_emissions <= upper_1st ~ 1,
      tot_emissions <= upper_2nd ~ 2,
      tot_emissions <= upper_3rd ~ 3,
      tot_emissions > upper_3rd ~ 4,
      TRUE ~ NA)
  )

## Total Emissions by Country Over Time
    # Perhaps this is only useful as a plotly graph
ggplot(plot_df %>% filter(Year >= 1900), #country_emissions_quartile == 4) , 
       aes(x = Year, y = Total, color = Country)) +
  geom_line(show.legend = FALSE) + 
  facet_wrap(~country_emissions_quartile,scales = 'free_y') + 
  scale_y_continuous(labels = scales::comma, n.breaks = 7) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_color_viridis_d(option = 'E') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 37)) +
  labs(
    title = 'C02 Emissions by Country over Time',
    subtitle = 'Year 1900 Onwards',
    y = 'Total: C02 Emissions' # Unsure the units, likely Kilotons according to wikipedia
  )

# Countries that emit the most C02 generally began emitting before the 1950s
  
## Coal, Oil, Gas, Cement, and Other as percentage of Total World Emissions over time
plot_df_2 <- mt_c02_adj %>%
  group_by(Year) %>%
  transmute(
    Year,
    `Global Total` = sum(Total, na.rm = T),
    `Global Coal` = sum(Coal, na.rm = T),
    `Global Oil` = sum(Oil, na.rm = T),
    `Global Gas` = sum(Gas, na.rm = T),
    `Global Cement` = sum(Cement, na.rm = T),
    `Global Flaring` = sum(Flaring, na.rm = T),
    `Global Other` = sum(Other, na.rm = T),
    coal = round(`Global Coal` * 100/ `Global Total`, 2),
    oil = round(`Global Oil` * 100 / `Global Total`, 2),
    gas = round(`Global Gas` * 100 / `Global Total`, 2),
    cement = round(`Global Cement` * 100 / `Global Total`, 2),
    flaring = round(`Global Flaring` * 100 / `Global Total`, 2),
    other = round(`Global Other` * 100 / `Global Total`, 2)
  ) %>% 
  distinct(Year, .keep_all = T) %>%
  pivot_longer(
    cols = c('coal', 'oil', 'gas', 'cement', 'flaring', 'other'),
    names_to = 'Emissions Category',
    values_to = 'percent'
    ) 

## Area plot of global emissions percent by category
plot_2 <- ggplot(plot_df_2 %>% filter(Year >= 1850), 
                 aes(x = Year, 
                     y = percent, 
                     fill = `Emissions Category`)) + 
  scale_x_continuous(expand = c(0.01, 0), n.breaks = 8) + 
  scale_y_continuous(expand = c(0, 0)) + 
  geom_area(size = 1, colour = "darkgrey", position = 'stack') + 
  scale_fill_viridis(discrete = T, option = 'E') +
  theme_minimal() + 
  labs(
    title = 'Global Emissions Percentage by Category',
    subtitle = 'Prior to 1850 Nearly All Emissions Were From Coal',
    x = 'Year',
    y = 'Percent'
  )

ggsave('global_percent_emissions_by_cat.svg', plot = plot_2,
       path = 'C:/Users/WulfN/R Projects/Emissions-Data-Analysis')
  
# Oil and Gas has contributed a greater percentage to Global emissions up through
# The 1970s, but has plateaued or is decreasing since then. 

## Area plot of absolute global emissions over time
plot_df_3 <- plot_df_2 %>%
  distinct(Year, .keep_all = T) %>%
  select(-c('Emissions Category', 'percent'),
         Coal = `Global Coal`,
         Oil = `Global Oil`,
         Gas = `Global Gas`,
         Cement = `Global Cement`,
         Flaring = `Global Flaring`,
         Other = `Global Other`
  ) %>%
  pivot_longer(
    cols = c('Coal', 'Oil', 'Gas', 'Cement',
             'Flaring', 'Other'),
    names_to = 'Global Emissions Category',
    values_to = 'Total Emissions'
  ) 

plot_3 <- ggplot(plot_df_3 %>% filter(Year >= 1850), 
                 aes(x = Year, 
                     y = `Total Emissions`/ 1000, # For cleaner y-axis
                     fill = `Global Emissions Category`)) + 
  scale_x_continuous(expand = c(0.01, 0), n.breaks = 8) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, .1), n.breaks = 7) + 
  geom_area(size = 1, colour = "darkgrey", position = 'stack') + 
  scale_fill_viridis(discrete = T, option = 'E') +
  theme_minimal() + 
  theme(axis.title.y = element_text(margin = margin(r = 12, unit = "pt")),
        legend.position = c(.14, .736)) +
  labs(
    title = 'Global Total Emissions by Category',
    subtitle = 'Omitting Years Prior to 1850',
    x = 'Year',
    y = 'Total Emissions (Billion Tonnes)'
  )

ggsave('global_total_emissions_by_cat.svg', plot = plot_3,
       path = 'C:/Users/WulfN/R Projects/Emissions-Data-Analysis')

  # Total Emissions has steadily risen, but after 2000 has arguably plateaued.
# Global Gas emissions has steadily increased, despite the total emissions trend. 
# Tonnes is equivalent to 1,000 Kilograms

# Modeling ----------------------------------------------------------------

# Time Series
# --- Clustering? 


# Results + Visualizations ------------------------------------------------

# This may be completed in the EDA section
# Choreographs! 
  # - Leaflet
  # - Different c02 emissions types?
  # - Show changes over time, or just most recent year? 

