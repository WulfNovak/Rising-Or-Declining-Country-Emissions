##################################################################
# Author: Wulf Novak
#
# Project: Emissions based - Will decide after EDA
#
# Data: World Emissions
#       https://zenodo.org/records/7215364
#       Convened by: Robbie M. Andrew and Glen Peters
#
#       World Development Indicators
#       https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators
#
# Date: 5/3/2024
#
# R version: 4.4.0
##################################################################
# Comments: 
#

# Setup -------------------------------------------------------------------

### Libraries
library(librarian)
shelf(tidyverse, data.table, future, furrr, 
      plotly, viridis, svglite)

options(scipen = 999)

### Read in Data 

## Country level Emissions - primary dataset for analysis
mt_c02 <- fread('./datasets/Emissions/GCB2023v43_MtCO2_flat.csv') %>%
  rename(Country_Code = `ISO 3166-1 alpha-3`)

# Per Capita Emissions
per_capita <- fread('./datasets/Emissions/GCB2023v43_percapita_flat.csv')

## World Development Indicators
wdi_country <- fread('./datasets/world_data/WDICountry.csv') %>% 
  select('Country Name' = `Country Code`, `Short Name`)

# hist data by year
wdi_csv <- fread('./datasets/world_data/WDICSV.csv', header = T) 

# Missing Values Function
  missing_vals <- function(dataframe) {
    
    require(purrr)
    require(dplyr)
    
    nrow <- nrow(dataframe)
    
    # Loop through each column and give stats on key missing value metrics
    results <- tibble(
      variables = colnames(dataframe),
      
      n_distnct = dataframe %>% 
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

# EDA ---------------------------------------------------------------------

# Missing values for dataframes
  missing_vals(mt_c02)
#   variables    n_distinct qty_na perc_na qty_blank perc_blank qty_zero perc_zero
# 1 Country             225      0     0           0          0        0      0   
# 2 Country_Code        221      0     0           0          0        0      0   
# 3 UN M49              225      0     0           0          0        0      0   
# 4 Year                273      0     0           0          0        0      0   
# 5 Total             17706  38253    62.3         0          0      257      0.42
# 6 Coal              12097  38470    62.6         0          0     6765     11.0 
# 7 Oil               11939  39655    64.6         0          0     2687      4.37
# 8 Gas                6547  39683    64.6         0          0    13895     22.6 
# 9 Cement             9227  37884    61.7         0          0    12319     20.1 
# 10 Flaring           4191  39774    64.8         0          0    17164     27.9 
# 11 Other             1565  59612    97.0         0          0      179      0.29
# 12 Per Capita       17215  43882    71.4         0          0      284      0.46
  
  missing_vals(per_capita)
#   variables          n_distinct qty_na perc_na qty_blank perc_blank qty_zero perc_zero
# 1 Country                   225      0     0           0          0        0      0   
# 2 ISO 3166-1 alpha-3        221      0     0           0          0        0      0   
# 3 UN M49                    225      0     0           0          0        0      0   
# 4 Year                      273      0     0           0          0        0      0   
# 5 Total                   17160  44092    71.8         0          0       57      0.09
# 6 Coal                    11091  44226    72           0          0     5861      9.54
# 7 Oil                     16026  44309    72.1         0          0      943      1.54
# 8 Gas                      7001  44399    72.3         0          0     9957     16.2 
# 9 Cement                   9877  46260    75.3         0          0     5022      8.18
# 10 Flaring                 4335  44500    72.4         0          0    12406     20.2 
# 11 Other                   1629  59758    97.3         0          0       33      0.05
  
# Data from 1750 Onwards for some countries  
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
          `Per Capita` = sum(`Per Capita`, na.rm = T)
         ) %>%
  distinct(Country, Year, Total, .keep_all = T)

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

# * No use for Country Code at this time

### Select development indicators of interest

# Writing csv of indicators to chatgpt to select categories to focus variable selection
devlop_indicators <- wdi_csv %>% distinct(`Indicator Name`) 
write_csv(devlop_indicators, file = './datasets/world_data/develop_indicators.csv')

### Read in categorized Data
cat_dev_indicators <- fread('./R Projects/Emissions-Data-Analysis/Data/Development_Indicators_Categorized.csv') %>%
  select( # reducing to the following categories for analysis
    `Public Services and Safety`,
    `Governance and Institutional Quality`,
    `Environmental Sustainability`,
    `Income and Poverty`,
    `Economic Performance and Growth`
  ) %>% 
  pivot_longer(cols = everything(), names_to = 'category', values_to = 'indicator') %>%
  filter(!indicator == '')


### Filtering out variables from each category

# Public Services and Safety
pub_ss <- cat_dev_indicators %>% filter(category == 'Public Services and Safety')
pub_ss <- pub_ss %>% filter(!grepl('^Annual Freshwater|^Net Bilateral|, urban|, rural',
                                   ignore.case = T, indicator))
# Governance and Institutional Quality
gov_iq <- cat_dev_indicators %>% filter(category == 'Governance and Institutional Quality')
gov_iq <- gov_iq %>% filter(!grepl(', female|, male|, older|, young|, poor|, rich|Lower Bound of 90%|upper Bound of 90%',
                                   ignore.case = T, indicator))
# Environmental Sustainability
env_sus <- cat_dev_indicators %>% filter(category == 'Environmental Sustainability')
env_sus <- env_sus %>% filter(!grepl('emissions|CO2', # prevent potential multicollinearity with emissions target variable
                                   ignore.case = T, indicator))
# Income and Poverty
inc_pov <- cat_dev_indicators %>% filter(category == 'Income and Poverty')
inc_pov <- inc_pov %>% filter(!grepl('current US$|^Merchandise|current LCU|Secondary income|Survey mean consumption', 
                                     ignore.case = T, indicator))
# Other considerations: income share of 20% groupings, 

# Economic Performance and Growth
econ_pg <- cat_dev_indicators %>% filter(category == 'Economic Performance and Growth')
econ_pg <- econ_pg %>% filter(!grepl(', male|, female|\\(current |emissions|NPISHs|hectares|^Claims on other sectors|Domestic credit|^Expense', 
                                     ignore.case = T, indicator))
# too many factors here, consider correlation matrix, then simple elastic net model to eliminate variables



### Filter by amount of data available per variable 

# look at correlation between variables, remove multicollinear



# Why not use Principal Component Analysis for dimensionality reduction.. ?

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
  ) %>%
  select(-c(starts_with('upper'), tot_emissions))

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
    y = 'Total C02 Emissions'
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

# Total Emissions has steadily risen, but has arguably plateaued after 2000.
# Global Gas emissions has steadily increased, despite the total emissions trend. 
# Tonnes is equivalent to 1,000 Kilograms


# Finding Countries That Successfully Reduced Emissions -------------------

# Definition: Have 3 consecutive years where emissions are less than the max

reduc_emissions_countries <- mt_c02_adj %>%
  group_by(Country) %>%
  mutate(
    max_total_emissions = max(Total, na.rm = T),
    # indicator for year of max emissions
    year_of_max_total_emissions = case_when(Total == max_total_emissions ~ 1, 
                                            TRUE ~ 0),
    most_recent_year = max(Year, na.rm = T)
  ) %>%
  filter(
    year_of_max_total_emissions == 1,
    # remove countries whose max emitting year is within past 3 years
    Year < max(Year, na.rm = T) - 3 
    ) %>%
  distinct(Country) %>%
  unlist(use.names = FALSE)

plot_reduc_emissions_countries <- plot_df %>%
  filter(Country %in% reduc_emissions_countries)

# Plot of countries with reduction in usage, split into quartiles 
ggplot(plot_reduc_emissions_countries %>% filter(Year >= 1900, country_emissions_quartile == 4), 
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
    y = 'Total C02 Emissions'
  )

# Look at countries whose usage has steadily risen

# Modeling ----------------------------------------------------------------

# Time Series
# --- Clustering? 


# Results + Visualizations ------------------------------------------------

# This may be completed in the EDA section
# Choreographs! 
  # - Leaflet
  # - Different c02 emissions types?
  # - Show changes over time, or just most recent year? 

