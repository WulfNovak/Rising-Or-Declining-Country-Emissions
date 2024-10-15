# ##############################################################################
# Author: Wulf Novak
#
# Project: Rising / Declining Country Emissions
#
# Date: 2024-05-03
#
# R version 4.4.0 (2024-04-24 ucrt)
# ------------------------------------------------------------------------------
# Sections:
# - Emissions Data Prep
# - Select Countries with Declining total Emissions
# - Select Countries with Rising total Emissions
# - Select Rising / Declining by Clustering
# - Modeling Dataframe 
# ##############################################################################

# Data: World Emissions
#       https://zenodo.org/records/7215364
#       Convened by: Robbie M. Andrew and Glen Peters
#

source('./R Projects/Emissions-Data-Analysis/Setup.R')

# Emissions Data Prep -----------------------------------------------------

## country level Emissions - primary dataset for analysis
emissions_dt <- fread('./datasets/Emissions/GCB2023v43_MtCO2_flat.csv') %>%
  rename(country_code = `ISO 3166-1 alpha-3`) %>%
  clean_names()

# country names (Cleaned) and Codes
wdi_country <- fread('./datasets/world_data/WDIcountry.csv') %>% 
  clean_names() %>%
  select(`country_code`, `short_name`)

# Missing values for dataframes
missing_vals(emissions_dt)
#   variables    n_distinct qty_na perc_na qty_blank perc_blank qty_zero perc_zero
# 1 country             225      0     0           0          0        0      0   
# 2 country_code        221      0     0           0          0        0      0   
# 3 un_m49              225      0     0           0          0        0      0   
# 4 year                273      0     0           0          0        0      0   
# 5 total             17706  38253    62.3         0          0      257      0.42
# 6 coal              12097  38470    62.6         0          0     6765     11.0 
# 7 oil               11939  39655    64.6         0          0     2687      4.37
# 8 gas                6547  39683    64.6         0          0    13895     22.6 
# 9 cement             9227  37884    61.7         0          0    12319     20.1 
# 10 flaring           4191  39774    64.8         0          0    17164     27.9 
# 11 other             1565  59612    97.0         0          0      179      0.29
# 12 per_capita       17215  43882    71.4         0          0      284      0.46

# missing_vals(per_capita)
#   variables          n_distinct qty_na perc_na qty_blank perc_blank qty_zero perc_zero
# 1 country                   225      0     0           0          0        0      0   
# 2 ISO 3166-1 alpha-3        221      0     0           0          0        0      0   
# 3 un_m49                    225      0     0           0          0        0      0   
# 4 year                      273      0     0           0          0        0      0   
# 5 total                   17160  44092    71.8         0          0       57      0.09
# 6 coal                    11091  44226    72           0          0     5861      9.54
# 7 oil                     16026  44309    72.1         0          0      943      1.54
# 8 gas                      7001  44399    72.3         0          0     9957     16.2 
# 9 cement                   9877  46260    75.3         0          0     5022      8.18
# 10 flaring                 4335  44500    72.4         0          0    12406     20.2 
# 11 other                   1629  59758    97.3         0          0       33      0.05

range(emissions_dt$year)
# 1750 to 2022

### Any country level stats to not consider? 

# emissions_dt %>% distinct(country) %>% View()
# International Transport, Global, Kuwait oil Fires.

# Adjust Japan emissions to include Ryukyu Islands emissions
# for the 1952-1972 time period they were reported separately.

japan_adj <- emissions_dt %>%
  filter(country %in% c('Ryukyu Islands', 'Japan')) %>%
  mutate(country = ifelse(country == 'Ryukyu Islands', 'Japan', country)) %>%
  group_by(country, year) %>%
  reframe(country, 
          country_code = 'JPN',
          year,
          total = sum(total, na.rm = T), 
          coal = sum(coal, na.rm = T),
          oil = sum(oil, na.rm = T),
          gas = sum(gas, na.rm = T),
          cement = sum(cement, na.rm = T),
          flaring = sum(flaring, na.rm = T),
          other = sum(other, na.rm = T),
          `per_capita` = sum(`per_capita`, na.rm = T)
  ) %>%
  distinct(country, year, total, .keep_all = T)

# Update country level emissions with Japan adjustment and remove non-countries
emissions_dt_2 <- emissions_dt %>%
  left_join(wdi_country, by = c('country_code' = 'country_code')) %>%
  filter(!country %in% c('International Transport', 'Global', 'Kuwaiti oil Fires', 
                         'Ryukyu Islands', 'Japan', 'International Shipping', 
                         'International Aviation', 'Leeward Islands',
                         'Pacific Islands (Palau)'
                         )
  ) %>%
  bind_rows(japan_adj) %>% 
  filter(!total == 0) %>% # Remove 0 total emission observations
  # The following countries had 0 emissions from 2000 onward:
  # 1. Christmas Island
  # 2. Panama Canal Zone
  mutate(country = 
           case_when(!is.na(country) & !is.na(`short_name`) ~ `short_name`, 
                     !is.na(`short_name`) ~ `short_name`,
                     is.na(`short_name`) ~ country)) %>% # Replace country name with accurate wdi country names
  select(-c('short_name', 'un_m49'))

# missing_vals(emissions_dt_2)

## Write emissions_dt_2 to datafile
#fwrite(emissions_dt_2, file = './datasets/Emissions/emissions_dt_2.rds')

### Analysis of total Emissions by country
country_mu_sd <- emissions_dt_2 %>% 
  group_by(country) %>%
  reframe(
    mean_total = mean(total, na.rm = T),
    sd_total = sd(total, na.rm = T)
    )

summary(country_mu_sd %>% select(-country))
#        mean_total           sd_total       
# Min.   :   0.0056   Min.   :   0.002  
# 1st Qu.:   0.6103   1st Qu.:   0.429  
# Median :   3.9728   Median :   3.627  
# Mean   :  49.4758   Mean   :  59.910  
# 3rd Qu.:  24.3463   3rd Qu.:  24.199  
# Max.   :2246.7168   Max.   :3311.293

# Select Countries with Declining total Emissions -------------------------

# Declining Emissions Definition: 
# 1. Max year of emissions cannot be 2017 onward 
#    (so that there is enough data to recognize a trend)
# 2. Slope of normalized total emissions by country to year is negative and 
#    statistically significant, for data of max year of emissions onward
# 3. Year >= 2000

reduc_emissions_countries <- emissions_dt_2 %>%
  select(country, year, total) %>%
  filter(year >= 2000) %>% 
  group_by(country) %>%
  mutate(
    max_total_emissions = max(total, na.rm = T),
    # indicator for year of max emissions
    year_of_max_total_emissions_indicator = case_when(total == max_total_emissions ~ 1, 
                                            TRUE ~ 0)
  ) %>%
  filter(
    year_of_max_total_emissions_indicator == 1,
    # remove countries whose max emitting year is within past 5 years,
    year < max(year, na.rm = T) - 5
  ) %>%
  mutate(# if multiple periods of emission decline, choose most recent
         year_of_max_total_emissions = max(year, na.rm = T)
         ) %>%
  ungroup() %>%
  distinct(country, year_of_max_total_emissions, .keep_all = T)

# 123 Countries omitted due to year requirement:
emissions_dt_2 %>% filter(!country %in% c(reduc_emissions_countries %>% pull(country))) %>% 
  distinct(country) %>% pull()

max_year_emissions <- reduc_emissions_countries %>% select(country, year_of_max_total_emissions)

### Finding slope and statistical significance
# By country, filter to data ranging from max year to 2022
# get slope based on year (as year increases, total emissions decrease)
# indicate countries whose emissions decrease

# min-max normalize data so that slopes and values can later be compared 
# between countries more easily
min_max_normalize <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

# Prepare data from year of max total emissions onward
emissions_slope_data <- emissions_dt_2 %>%
  left_join(max_year_emissions, by = 'country') %>%
  select(country, year, year_of_max_total_emissions, total, per_capita) %>%
  group_by(country) %>%
  filter(# remove countries omitted by year requirement from consideration
         country %in% c(max_year_emissions %>% pull(country)),
         year >= year_of_max_total_emissions) %>%
  mutate(norm_tot = min_max_normalize(total)) # min-max normalized of total within country

# Create models and summaries for each country, get significant slopes

sig_slope <- function(data){
  
  model_summaries <- data %>%
    group_by(country) %>%
    nest() %>%
    reframe(
      model = map(data, .f = ~ lm(norm_tot ~ year, data = .x)),
      summary = map(model, broom::tidy)
    )
  
  # Unpack critical information (country, slope estimate, p-value, significance)
  model_stats <- model_summaries %>%
    select(country, summary) %>%
    unnest(summary) %>%
    group_by(country) %>%
    reframe(
      slope_estimate = round(estimate[2], 3),
      p_value = format.pval(p.value[2], digits = 3),
      sig_alpha_.05 = case_when(p_value <= .05 ~ 1,
                                p_value > .05 ~ 0)
    ) %>% 
    ungroup() %>%
    distinct(country, .keep_all = T)
  
  return(model_stats)
}

decline_stats <- sig_slope(emissions_slope_data)

# Count of countries with a statistically significant slope estimate: 72
count(decline_stats, sig_alpha_.05)
#   sig_alpha_.05     n
# 1             0    35
# 2             1    60

# Countries with significant AND negative slope - Keepers!
decline_stats %>% filter(slope_estimate < 0, sig_alpha_.05 == 1) %>% pull(country)
# [1] "Andorra"                   "Angola"            "Aruba"                     "Austria"                  
# [5] "Barbados"                  "Belgium"           "Brazil"                    "British Virgin Islands"   
# [9] "Bulgaria"                  "Cook Islands"      "Croatia"                   "Cuba"                     
# [13] "Curaçao"                   "Cyprus"           "Czechia"                   "Denmark"                  
# [17] "Equatorial Guinea"         "Estonia"          "Finland"                   "France"                   
# [21] "Germany"                   "Greece"           "Hong Kong SAR, China"      "Hungary"                  
# [25] "Ireland"                   "Israel"           "Italy"                     "Jamaica"                  
# [29] "Japan"                     "Jordan"           "Latvia"                    "Liechtenstein"            
# [33] "Lithuania"                 "Luxembourg"       "Malta"                     "Montserrat"               
# [37] "Nauru"                     "Netherlands"      "North Macedonia"           "Norway"                   
# [41] "Portugal"                  "Romania"          "Serbia"                    "Sint Maarten (Dutch part)"
# [45] "Slovak Republic"           "Slovenia"         "South Africa"              "Spain"                    
# [49] "Sri Lanka"                 "Sweden"           "Switzerland"               "Syrian Arab Republic"     
# [53] "Trinidad and Tobago"       "Ukraine"          "United Kingdom"            "United States"            
# [57] "Uzbekistan"                "Venezuela"        "Yemen"                     "Zimbabwe"

final_declining_emissions <- decline_stats %>% 
  filter(slope_estimate < 0, sig_alpha_.05 == 1) %>%
  select(country, 
         slope_estimate_declining = slope_estimate, 
         sig_alpha_.05_declining = sig_alpha_.05)

# 60 countries total with declining emissions

# Select Countries with Rising total Emissions ----------------------------

# Of countries without declining total emissions, which have rising total emissions?
rising_emissions_candidates <- emissions_dt_2 %>%
  select(country, year, total) %>%
  filter(!country %in% final_declining_emissions$country) %>%
  group_by(country) %>%
  mutate(
    max_total_emissions = max(total, na.rm = T),
    # indicator for year of max emissions
    year_of_max_total_emissions_indicator = case_when(total == max_total_emissions ~ 1, 
                                                      TRUE ~ 0)) %>%
  filter(year_of_max_total_emissions_indicator == 1) %>%
  mutate(# if multiple periods of max emissions, choose most recent
    year_of_max_total_emissions = max(year, na.rm = T)
  ) %>%
  ungroup() %>%
  distinct(country, year_of_max_total_emissions)

# prepare country-level slope data
rising_emissions_slope_data <- emissions_dt_2 %>%
  left_join(rising_emissions_candidates, by = 'country') %>%
  select(country, year, year_of_max_total_emissions, total) %>%
  group_by(country) %>%
  filter(
    # remove countries omitted by year requirement from consideration
    country %in% c(rising_emissions_candidates %>% pull(country)),
    # Calculate slope from minimum of 1. year with max emissions OR 2. 2000
      # To assure plentiful data for countries whose max emissions year is > 2000
    year >= case_when(year_of_max_total_emissions < 2000 ~ year_of_max_total_emissions,
                      year_of_max_total_emissions >= 2000 ~ 2000),
    # remove countries whose total emissions are 0 after 2000
    case_when(year >= 2000 & total != 0 ~ TRUE,
              year >= 2000 & total == 0 ~ FALSE)
      # The following countries are removed: 
      # 1. "Panama Canal Zone"
      # 2. "Leeward Islands"
      # 3. "Pacific Islands (Palau)"
    ) %>%
  mutate(norm_tot = min_max_normalize(total)) %>%
  ungroup()

rising_stats <- sig_slope(rising_emissions_slope_data)

# Count of countries with significant slopes
count(rising_stats , sig_alpha_.05)
#   sig_alpha_.05     n
# 1             0    15
# 2             1   140

# What countries do not have significant slopes? 
  # What are their average emissions from 2000 onward - slope? p-value?
rising_emissions_slope_data %>% 
  right_join(rising_stats, by = 'country') %>% 
  group_by(country) %>%
  filter(year >= 2000,
         sig_alpha_.05 == 0) %>%
  reframe(avg_emissions = mean(total),
          slope_estimate,
          p_value) %>%
  distinct()

#   country                   avg_emissions slope_estimate p_value
# 1 Bermuda                          0.560          -0.023 0.477  
# 2 Central African Republic         0.205          -0.016 0.625  
# 3 Cuba                            26.3            -0.053 0.0941 
# 4 Djibouti                         0.434           0.031 0.337  
# 5 Eritrea                          0.604           0.001 0.981  
# 6 Eswatini                         1.06            0.013 0.686  
# 7 Faroe Islands                    0.680          -0.001 0.985  
# 8 Gabon                            5.93           -0.039 0.222  
# 9 Libya                           55.7             0.036 0.257  
# 10 Micronesia                      0.139          -0.009 0.793  
# 11 New Zealand                    35.3            -0.022 0.493  
# 12 Palau                           0.217           0.003 0.921  
# 13 Saint Pierre and Miquelon       0.0631          0.051 0.104  
# 14 Singapore                      46.6             0.03  0.348  
# 15 Wallis and Futuna Islands       0.0259         -0.029 0.376 
  
# *** Will omit the above countries to better distinguish countries with 
#     rising vs declining emissions.

# What countries have significant slopes, but need to be removed due to negative slope?
  # avg emissions 2000 onward? slope? p-value?
rising_emissions_slope_data %>% 
  right_join(rising_stats, by = 'country') %>% 
  group_by(country) %>%
  filter(year >= 2000,
         sig_alpha_.05 == 1,
         slope_estimate <= 0) %>%
  reframe(avg_emissions = mean(total),
          slope_estimate,
          p_value) %>%
  distinct()

#   country                  avg_emissions slope_estimate p_value 
# 1 Canada                         565.0            -0.014 0.0391  
# 2 Dem. People's Rep. Korea        53.4           -0.031 0.000916
# 3 Greenland                        0.602         -0.027 0.00139 
# 4 Macao SAR, China                 1.43          -0.022 0.0262

# *** Will omit for now. these slopes on normalized emissions are nearly 0

# What countries have statistically significant rising emissions?
rising_stats %>% 
  filter(sig_alpha_.05 == 1, slope_estimate > 0) %>%
  pull(country)

# [1] "Afghanistan"                       "Albania"                           "Algeria"                          
# [4] "Anguilla"                          "Antarctica"                        "Antigua and Barbuda"              
# [7] "Argentina"                         "Armenia"                           "Australia"                        
# [10] "Azerbaijan"                        "Bahrain"                           "Bangladesh"                       
# [13] "Belarus"                           "Belize"                            "Benin"                            
# [16] "Bhutan"                            "Bolivia"                           "Bonaire, Saint Eustatius and Saba"
# [19] "Bosnia and Herzegovina"            "Botswana"                          "Brunei"                           
# [22] "Burkina Faso"                      "Burundi"                           "Cabo Verde"                       
# [25] "Cambodia"                          "Cameroon"                          "Chad"                             
# [28] "Chile"                             "China"                             "Colombia"                         
# [31] "Comoros"                           "Congo"                             "Costa Rica"                       
# [34] "Côte d'Ivoire"                     "Dem. Rep. Congo"                   "Dominica"                         
# [37] "Dominican Republic"                "Ecuador"                           "Egypt"                            
# [40] "El Salvador"                       "Ethiopia"                          "Fiji"                             
# [43] "French Polynesia"                  "Georgia"                           "Ghana"                            
# [46] "Grenada"                           "Guatemala"                         "Guinea"                           
# [49] "Guinea-Bissau"                     "Guyana"                            "Haiti"                            
# [52] "Honduras"                          "Iceland"                           "India"                            
# [55] "Indonesia"                         "Iran"                              "Iraq"                             
# [58] "Kazakhstan"                        "Kenya"                             "Kiribati"                         
# [61] "Korea"                             "Kosovo"                            "Kuwait"                           
# [64] "Kyrgyz Republic"                   "Lao PDR"                           "Lebanon"                          
# [67] "Lesotho"                           "Liberia"                           "Madagascar"                       
# [70] "Malawi"                            "Malaysia"                          "Maldives"                         
# [73] "Mali"                              "Marshall Islands"                  "Mauritania"                       
# [76] "Mauritius"                         "Mexico"                            "Moldova"                          
# [79] "Mongolia"                          "Montenegro"                        "Morocco"                          
# [82] "Mozambique"                        "Myanmar"                           "Namibia"                          
# [85] "Nepal"                             "New Caledonia"                     "Nicaragua"                        
# [88] "Niger"                             "Nigeria"                           "Niue"                             
# [91] "Oman"                              "Pakistan"                          "Panama"                           
# [94] "Papua New Guinea"                  "Paraguay"                          "Peru"                             
# [97] "Philippines"                       "Qatar"                             "Russia"                           
# [100] "Rwanda"                            "Saint Helena"                      "Samoa"                            
# [103] "Saudi Arabia"                      "Senegal"                           "Seychelles"                       
# [106] "Sierra Leone"                      "Solomon Islands"                   "Somalia"                          
# [109] "South Sudan"                       "St. Kitts and Nevis"               "St. Lucia"                        
# [112] "St. Vincent and the Grenadines"    "Sudan"                             "Suriname"                         
# [115] "São Tomé and Principe"             "Taiwan"                            "Tajikistan"                       
# [118] "Tanzania"                          "Thailand"                          "The Bahamas"                      
# [121] "The Gambia"                        "Timor-Leste"                       "Togo"                             
# [124] "Tonga"                             "Tunisia"                           "Turkmenistan"                     
# [127] "Turks and Caicos Islands"          "Tuvalu"                            "Türkiye"                          
# [130] "Uganda"                            "United Arab Emirates"              "Uruguay"                          
# [133] "Vanuatu"                           "Viet Nam"                          "West Bank and Gaza"               
# [136] "Zambia"  

final_rising_emissions <- rising_stats %>% 
  filter(sig_alpha_.05 == 1, slope_estimate > 0) %>%
  select(country, 
         slope_estimate_rising = slope_estimate, 
         sig_alpha_.05_rising = sig_alpha_.05)

# Select Rising / Declining by Clustering ---------------------------------

# Rather than using slopes, utilize longitudinal clustering algorithm,
# that will cluster by time series trends.

# Data prep
clust_data <- emissions_dt_2 %>%
  select(country, year, total, per_capita) %>%
  filter(year >= 2000) %>%
  group_by(country) %>%
  mutate(norm_tot = min_max_normalize(total)) %>% # min-max normalized of total within country
  ungroup()

clust_data_nt <- clust_data %>%
  select(-per_capita, -total) %>% 
  pivot_wider(names_from = year,
              values_from = norm_tot)

clust_data_pc <- clust_data %>%
  select(-total, -norm_tot) %>%
  pivot_wider(names_from = year, 
              values_from = per_capita)

# Longitudinal k-means (KML) Clustering
librarian::shelf(kml)

# Format data for kml
clust_data_norm_tot <- cld(idAll = clust_data_nt %>% pull(country),
                           traj = clust_data_nt %>% select(-country))

clust_data_per_capita <- cld(idAll = clust_data_pc %>% pull(country),
                             traj = clust_data_pc %>% select(-country))

## cluster on normalized total emissions usage
set.seed(123)
clust_norm_tot <- kml(clust_data_norm_tot, nbClusters = 2, 
                      nbRedrawing = 20, toPlot = "traj")

# Selecting cluster with lowest Calinksi Harabatz
clusters_nt <- getClusters(clust_data_norm_tot, nbCluster = 2, 20)

# cluster on per capita emissions usage
set.seed(123)
clust_per_capita <- kml(clust_data_per_capita, nbClusters = 2, 
                        nbRedrawing = 20, toPlot = "traj")

# Selecting cluster with lowest Calinksi Harabatz
clusters_pc <- getClusters(clust_data_per_capita, nbCluster = 2, 20)

## Clustering Results
clust_results <- tibble(
  country = clust_data_nt %>% pull(country),
  nt_clust = clusters_nt
) %>%
  mutate(nt_clust = case_when(nt_clust == 'A' ~ 'non_declining',
                              nt_clust == 'B' ~ 'declining'))

# A = non-declining emissions, B = declining emissions
count(clust_results, nt_clust)
#   nt_clust          n
# 1 declining        66
# 2 non_declining   149

# Final Dataframe ---------------------------------------------------------

# Adding slope estimates for total and per capita:

# For interpretable world plots
plot_model_summaries <- emissions_dt_2 %>%
  select(country, year, total, per_capita) %>%
  filter(year >= 2000) %>%
  group_by(country) %>%
  nest() %>%
  reframe(
    model_tot = map(data, .f = ~ lm(total ~ year, data = .x)),
    summary_tot = map(model_tot, broom::tidy),
    model_per_capita = map(data, .f = ~ lm(per_capita ~ year, data = .x)),
    summary_per_capita = map(model_per_capita, broom::tidy)
  )

# slope of total and per capita
tot_model_stats <- plot_model_summaries %>%
  select(country, summary_tot) %>%
  unnest(summary_tot) %>%
  group_by(country) %>%
  reframe(
    tot_slope_estimate = round(estimate[2], 3)
  ) %>% 
  ungroup() %>%
  distinct(country, .keep_all = T)

per_capita_model_stats <- plot_model_summaries %>%
  select(country, summary_per_capita) %>%
  unnest(summary_per_capita) %>%
  group_by(country) %>%
  reframe(
    per_capita_slope_estimate = round(estimate[2], 3)
  ) %>% 
  ungroup() %>%
  distinct(country, .keep_all = T)

# Combine emissions and indicator data
final_df <- emissions_dt_2 %>%
  # Removing the following variables from scope
  select(-c('cement', 'flaring', 'other')) %>%
  filter(year >= 2000) %>%
  mutate(declining_emissions_indicator = 
           case_when(country %in% final_declining_emissions$country ~ 'declining',
                     country %in% final_rising_emissions$country ~ 'rising',
                     TRUE ~ 'insignificant_trend') %>%
           as.factor()) %>%
  group_by(country) %>%
  # min-max normalization of total by country
  left_join(final_rising_emissions, by = 'country') %>%
  left_join(final_declining_emissions, by = 'country') %>%
  left_join(tot_model_stats, by = 'country') %>%
  left_join(per_capita_model_stats, by = 'country') %>%
  left_join(clust_results, by = 'country') %>%
  mutate(country = as.factor(country), 
         norm_tot = round(min_max_normalize(total) * 100, 2),
         clust_slope_decline = 
           case_when(declining_emissions_indicator == 'declining' & nt_clust == 'declining' ~ 'declining',
                     TRUE ~ 'non-declining')
         ) %>%
  ungroup() 

count(final_df %>% distinct(country, .keep_all = T), declining_emissions_indicator)
#   declining_emissions_indicator     n
# 1 declining                        60
# 2 insignificant_trend              19
# 3 rising                          136

count(final_df %>% distinct(country, .keep_all = T), nt_clust)
#   nt_clust          n
# 1 declining        66
# 2 non_declining   149

# Both clust and slope method give declining emissions
count(final_df %>% distinct(country, .keep_all = T), clust_slope_decline)
#   clust_slope_decline     n
# 1 declining              50
# 2 non-declining         165

# Save modeling_df
fwrite(final_df, file = './datasets/Emissions/final_df.rds')














