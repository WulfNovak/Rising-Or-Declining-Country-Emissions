# ##############################################################################
# Author: Wulf Novak
#
# Project: Development Indicators of Rising 
#          and Declining Country Emissions
#
# Date: 2024-05-03
#
# R version 4.4.0 (2024-04-24 ucrt)
# ------------------------------------------------------------------------------
# Sections:
# - Emissions Data Prep
# - World Development Indicators Prep
# - Select Countries with Declining Total Emissions
# - Select Countries with Rising Total Emissions
# ##############################################################################

# Data: World Emissions
#       https://zenodo.org/records/7215364
#       Convened by: Robbie M. Andrew and Glen Peters
#
#       World Development Indicators (wdi)
#       https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators

source('./R Projects/Emissions-Data-Analysis/Setup.R')

# Emissions Data Prep -----------------------------------------------------

## Country level Emissions - primary dataset for analysis
emissions_dt <- fread('./datasets/Emissions/GCB2023v43_MtCO2_flat.csv') %>%
  rename(Country_Code = `ISO 3166-1 alpha-3`)

# Country Names (Cleaned) and Codes
wdi_country <- fread('./datasets/world_data/WDICountry.csv') %>% 
  select(`Country Code`, `Short Name`)

## Per Capita Emissions - Currently Unneeded
# per_capita <- fread('./datasets/Emissions/GCB2023v43_percapita_flat.csv')

# Missing values for dataframes
missing_vals(emissions_dt)
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

# missing_vals(per_capita)
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

range(emissions_dt$Year)
# 1750 to 2022

### Any 'Country' level stats to not consider? 

# emissions_dt %>% distinct(Country) %>% View()
# International Transport, Global, Kuwait Oil Fires.

# Adjust Japan emissions to include Ryukyu Islands emissions
# for the 1952-1972 time period they were reported separately.

japan_adj <- emissions_dt %>%
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
emissions_dt_2 <- emissions_dt %>%
  left_join(wdi_country, by = c('Country_Code' = 'Country Code')) %>%
  filter(!Country %in% c('International Transport', 'Global', 'Kuwaiti Oil Fires', 
                         'Ryukyu Islands', 'Japan', 'International Shipping', 
                         'International Aviation', 'Leeward Islands',
                         'Pacific Islands (Palau)'
                         )
  ) %>%
  bind_rows(japan_adj) %>% 
  filter(!Total == 0) %>% # Remove 0 Total emission observations
  # The following countries had 0 emissions from 2000 onward:
  # 1. Christmas Island
  # 2. Panama Canal Zone
  mutate(Country = 
           case_when(!is.na(Country) & !is.na(`Short Name`) ~ `Short Name`, 
                     !is.na(`Short Name`) ~ `Short Name`,
                     is.na(`Short Name`) ~ Country)) %>% # Replace country name with accurate wdi country names
  select(-c('Short Name', 'UN M49'))

# missing_vals(emissions_dt_2)

## Write emissions_dt_2 to datafile
fwrite(emissions_dt_2, file = './datasets/Emissions/emissions_dt_2.rds')

### Analysis of Total Emissions by Country
country_mu_sd <- emissions_dt_2 %>% 
  group_by(Country) %>%
  reframe(
    mean_total = mean(Total, na.rm = T),
    sd_total = sd(Total, na.rm = T)
    )

summary(country_mu_sd %>% select(-Country))
#        mean_total           sd_total       
# Min.   :   0.0056   Min.   :   0.002  
# 1st Qu.:   0.6103   1st Qu.:   0.429  
# Median :   3.9728   Median :   3.627  
# Mean   :  49.4758   Mean   :  59.910  
# 3rd Qu.:  24.3463   3rd Qu.:  24.199  
# Max.   :2246.7168   Max.   :3311.293

# World Development Indicators Prep ---------------------------------------

## Development Indicators by Year
wdi_csv <- fread('./datasets/world_data/WDICSV.csv', header = T) %>%
  filter(`Country Code` %in% emissions_dt_2$Country_Code)

# Countries/islands omitted due to lack of development indicator data
emissions_dt_2 %>% 
  filter(!Country_Code %in% wdi_country$`Country Code`,
         Year >= 2000) %>% #
  group_by(Country) %>%
  mutate(mean_total_emissions = round(mean(Total, na.rm = T), 2)) %>%
  ungroup() %>%
  distinct(Country, mean_total_emissions)

# Country                           mean_total_emissions
# 1 Anguilla                                          0.12
# 2 Antarctica                                        0.01
# 3 Bonaire, Saint Eustatius and Saba                 0.08
# 4 Cook Islands                                      0.07
# 5 Kosovo                                            6.41
# 6 Montserrat                                        0.04
# 7 Niue                                              0.01
# 8 Saint Helena                                      0.01
# 9 Saint Pierre and Miquelon                         0.06
# 10 Taiwan                                         267.40  
# 11 Wallis and Futuna Islands                        0.03

### Select development indicators of interest

# # Writing csv of indicators to chatgpt to select categories for variable selection
# develop_indicators <- wdi_csv %>% distinct(`Indicator Name`) 
# write_csv(develop_indicators, file = './datasets/world_data/develop_indicators.csv')

### Read in categorized Data

# ** Categories and descriptions are on github within 'Data' **

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

# Economic Performance and Growth
econ_pg <- cat_dev_indicators %>% filter(category == 'Economic Performance and Growth')
econ_pg <- econ_pg %>% filter(!grepl(', male|, female|\\(current |emissions|NPISHs|hectares|^Claims on other sectors|Domestic credit|^Expense', 
                                     ignore.case = T, indicator))
# too many factors here, consider simple elastic net model to eliminate variables

### Filter wdi_csv dataframe by development indicator
all_ind_filt <- bind_rows(pub_ss, gov_iq, env_sus, inc_pov, econ_pg)

# Saving to Reference later
fwrite(all_ind_filt, file = './R Projects/Emissions-Data-Analysis/Data/Filtered_Indicators_Categorized.csv')

# Filtering longitudinal indicator data 
wdi_filtered <- wdi_csv %>% 
  filter(`Indicator Name` %in% all_ind_filt$indicator)

missing_vals(wdi_filtered)
# Coverage improves drastically from 2000 onward. 
# Will filter to indicators from year 2000 forward.

# Save space
rm(wdi_csv)
gc()

# ### Filter by amount of data available per variable 
missing_indicators <- wdi_filtered %>%
  mutate(na_count_row = rowSums(is.na(.)),
         indicator_name = `Indicator Name`) %>%
  group_by(indicator_name) %>%
  # max possible NA: 2023 - 1960 = 63
  reframe(
    indicator_name,
    count = n(), # sum characters
    avg_na = mean(na_count_row, na.rm = T)
  )

# dist'n of missing values
ggplot(missing_indicators, aes(x = avg_na)) +
  geom_density()

# eliminate indicators where there are 60 + missing values (50 + ?)
many_missing <- missing_indicators %>%
  distinct(indicator_name, .keep_all = T) %>%
  filter(avg_na > 60) %>%
  pull(indicator_name)
# 
# # [1] "Account ownership at a financial institution or with a mobile-money-service provider (% of population ages 15+)"  
# # [2] "Annualized average growth rate in per capita real survey mean consumption or income, bottom 40% of population (%)"
# # [3] "Annualized average growth rate in per capita real survey mean consumption or income, total population (%)"        
# # [4] "Child employment in agriculture (% of economically active children ages 7-14)"                                    
# # [5] "Child employment in manufacturing (% of economically active children ages 7-14)"                                  
# # [6] "Child employment in services (% of economically active children ages 7-14)"                                       
# # [7] "Informal payments to public officials (% of firms)"                                                               
# # [8] "Investment in water and sanitation with private participation (current US$)"                                      
# # [9] "Multidimensional poverty headcount ratio (UNDP) (% of population)"                                                
# # [10] "Present value of external debt (% of exports of goods, services and primary income)"                              
# # [11] "Public private partnerships investment in ICT (current US$)"                                                      
# # [12] "Public private partnerships investment in transport (current US$)"                                                
# # [13] "Public private partnerships investment in water and sanitation (current US$)" 

wdi_filtered_2 <- wdi_filtered %>% 
  filter(!`Indicator Name` %in% many_missing) %>%
  # omitting 2023 because development indicators are only to year 2022
  select(-c(`Indicator Code`, `2023`)) %>%
  rename(Country_Name = `Country Name`,
         Country_Code = `Country Code`,
         Indicator_Name = `Indicator Name`) %>%
  pivot_longer(cols = `1960`:`2022`, names_to = "Year") %>%
  filter(Year >= 2000) %>%
  # perc NAs by indicator
  group_by(Country_Name, Indicator_Name) %>%
  mutate(perc_na = round(sum(is.na(value) * 100) / n(), 2)) %>%
  ungroup() %>%
  group_by(Indicator_Name) %>%
  mutate(global_perc_na = round(mean(perc_na, na.rm = T), 2)) %>%
  ungroup()
  
# How complete should the development indicators be across countries?? 
indicator_global_perc_na <- wdi_filtered_2 %>% 
  distinct(Indicator_Name, global_perc_na) %>% 
  arrange(desc(global_perc_na))

summary(indicator_global_perc_na$global_perc_na)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02    9.51   24.98   33.01   55.27   87.30 

# Unsure practical data coverage cutoff.
ggplot(indicator_global_perc_na, aes(x = global_perc_na)) +
  geom_density() # bimodal dist'n 

missing_vals(wdi_filtered_2)

rm(wdi_filtered)
gc()

### Num groups, num features, num df
n_distinct(wdi_filtered_2$Country_Name) 
# 204 countries

n_distinct(wdi_filtered_2$Indicator_Name) 
# 183 indicators

# Method for selecting highly coorelated variables:
# ** Do for both Rising / Declining countries
# For all development indicators, find country with greatest coverage
# Reduce variable to available NAs (hopefully there is a lot of coverage)
# eliminate variables that have low absolute value correlation for BOTH 
# rising / declining countries

# row NA sums? remove Indicator code
# sig diff between countries that have rising vs declining emissions 
# (each country is an observation)
# Perhaps look at peak year of development indicators?
# Drastically better coverage of development indicators from 2000 onwards


# Select Countries with Declining Total Emissions -------------------------

# Declining Emissions Definition: 
# 1. Max year of emissions cannot be 2019 onward 
#    (so that there is enough data to recognize a trend)
# 2. Slope of normalized total emissions by country to year is negative and 
#    statistically significant, for data of max year of emissions onward

reduc_emissions_countries <- emissions_dt_2 %>%
  select(Country, Year, Total) %>%
  group_by(Country) %>%
  mutate(
    max_total_emissions = max(Total, na.rm = T),
    # indicator for year of max emissions
    year_of_max_total_emissions_indicator = case_when(Total == max_total_emissions ~ 1, 
                                            TRUE ~ 0)
  ) %>%
  filter(
    year_of_max_total_emissions_indicator == 1,
    # remove countries whose max emitting year is within past 3 years,
    Year < max(Year, na.rm = T) - 3 # *** Omits 91 countries
  ) %>%
  mutate(# if multiple periods of emission decline, choose most recent
         year_of_max_total_emissions = max(Year, na.rm = T)
         ) %>%
  ungroup() %>%
  distinct(Country, year_of_max_total_emissions, .keep_all = T)

# 87 Countries omitted due to year requirement:
emissions_dt_2 %>% filter(!Country %in% c(reduc_emissions_countries %>% pull(Country))) %>% 
  distinct(Country) %>% pull()

# [1] "Afghanistan"          "Algeria"              "Anguilla"            
# [4] "Argentina"            "Australia"            "Bahrain"             
# [7] "Bangladesh"           "Belize"               "Benin"               
# [10] "Burkina Faso"         "Burundi"              "Cambodia"            
# [13] "Chad"                 "Chile"                "China"               
# [16] "Colombia"             "Comoros"              "Congo"               
# [19] "Dominican Republic"   "El Salvador"          "Ethiopia"            
# [22] "Fiji"                 "Panama Canal Zone"    "The Gambia"          
# [25] "Ghana"                "Grenada"              "Guatemala"           
# [28] "Guinea"               "Guinea-Bissau"        "Guyana"              
# [31] "Honduras"             "India"                "Indonesia"           
# [34] "Iraq"                 "Kenya"                "Kiribati"            
# [37] "Kuwait"               "Lao PDR"              "Lesotho"             
# [40] "Libya"                "Madagascar"           "Malawi"              
# [43] "Malaysia"             "Maldives"             "Mali"                
# [46] "Marshall Islands"     "Mauritania"           "Mexico"              
# [49] "Mongolia"             "Morocco"              "Myanmar"             
# [52] "Nepal"                "Nicaragua"            "Niger"               
# [55] "Nigeria"              "West Bank and Gaza"   "Oman"                
# [58] "Pakistan"             "Panama"               "Papua New Guinea"    
# [61] "Paraguay"             "Peru"                 "Philippines"         
# [64] "Bolivia"              "Qatar"                "Cameroon"            
# [67] "Sudan"                "Rwanda"               "Saint Helena"        
# [70] "Samoa"                "Saudi Arabia"         "Senegal"             
# [73] "Seychelles"           "St. Kitts and Nevis"  "Suriname"            
# [76] "Taiwan"               "Tonga"                "Tunisia"             
# [79] "Türkiye"              "Turkmenistan"         "Tuvalu"              
# [82] "Uganda"               "United Arab Emirates" "Tanzania"            
# [85] "Vanuatu"              "Viet Nam"             "Zambia" 

max_year_emissions <- reduc_emissions_countries %>% select(Country, year_of_max_total_emissions)

### Finding slope and statistical significance
# By country, filter to data ranging from max year to 2022
# get slope based on year (as year increases, total emissions decrease)
# indicate countries whose emissions do NOT decrease

# Prepare data from year of max total emissions onward
emissions_slope_data <- emissions_dt_2 %>%
  left_join(max_year_emissions, by = 'Country') %>%
  select(Country, Year, year_of_max_total_emissions, Total) %>%
  group_by(Country) %>%
  filter(# remove countries omitted by year requirement from consideration
         Country %in% c(max_year_emissions %>% pull(Country)),
         Year >= year_of_max_total_emissions) %>%
  mutate(norm_tot = scale(Total)[,1]) # normalize total by year 

# Create models and summaries for each country
model_summaries <- emissions_slope_data %>%
  group_by(Country) %>%
  nest() %>%
  reframe(
    model = map(data, .f = ~ lm(norm_tot ~ Year, data = .x)),
    summary = map(model, broom::tidy)
  )

# Unpack critical information (Country, slope estimate, p-value, significance)
model_stats <- model_summaries %>%
  select(Country, summary) %>%
  unnest(summary) %>%
  group_by(Country) %>%
  reframe(
    slope_estimate = round(estimate[2], 3),
    p_value = format.pval(p.value[2], digits = 3),
    sig_alpha_.05 = case_when(p_value <= .05 ~ 1,
                              p_value > .05 ~ 0)
  ) %>% 
  ungroup() %>%
  distinct(Country, .keep_all = T)

# Count of countries with a statistically significant slope estimate: 72
count(model_stats, sig_alpha_.05)
#   sig_alpha_.05     n
# 1             0    58
# 2             1    72

# Countries omitted due to insignificant slope OR positive slope
model_stats %>% filter(slope_estimate > 0 | sig_alpha_.05 == 0) %>% pull(Country)

# [1] "Albania"         "Antarctica"               "Antigua and Barbuda"              
# [4] "Armenia"         "Azerbaijan"               "Bermuda"                          
# [7] "Bhutan"          "Uruguay"                  "Bosnia and Herzegovina"           
# [10] "Botswana"       "Brunei"                   "Cabo Verde"                       
# [13] "Canada"         "Central African Republic" "Costa Rica"                       
# [16] "Cuba"           "Côte d'Ivoire"            "Dem. Rep. Congo"                  
# [19] "Djibouti"       "Dominica"                 "Ecuador"                          
# [22] "Egypt"          "Eritrea"                  "Eswatini"                         
# [25] "Faroe Islands"  "French Polynesia"         "Gabon"                            
# [28] "Georgia"        "Greenland"                "Iceland"                          
# [31] "Iran"           "Kazakhstan"               "Korea"                            
# [34] "Kosovo"         "Liberia"                  "Macao SAR, China"                 
# [37] "Mauritius"      "Micronesia"               "Montenegro"                       
# [40] "Mozambique"     "Namibia"                  "Netherlands"                      
# [43] "New Caledonia"  "New Zealand"              "Niue"                             
# [46] "Palau"          "Russia"                   "Saint Pierre and Miquelon"        
# [49] "Sierra Leone"   "Singapore"                "Solomon Islands"                  
# [52] "Somalia"        "South Sudan"              "St. Vincent and the Grenadines"   
# [55] "Switzerland"    "São Tomé and Principe"    "Wallis and Futuna Islands"                     
# [58] "Timor-Leste"    "Togo"                     "Bonaire, Saint Eustatius and Saba"                          
# [61] "Tajikistan"  

# Countries with significant AND negative slope - Keepers!
model_stats %>% filter(slope_estimate < 0, sig_alpha_.05 == 1) %>% pull(Country)
# [1] "Andorra"                   "Angola"                    "Aruba"                    
# [4] "Austria"                   "Barbados"                  "Belarus"                  
# [7] "Belgium"                   "Brazil"                    "British Virgin Islands"   
# [10] "Bulgaria"                  "Christmas Island"          "Cook Islands"             
# [13] "Croatia"                   "Curaçao"                   "Cyprus"                   
# [16] "Czechia"                   "Dem. People's Rep. Korea"  "Denmark"                  
# [19] "Equatorial Guinea"         "Estonia"                   "Finland"                  
# [22] "France"                    "Germany"                   "Greece"                   
# [25] "Haiti"                     "Hong Kong SAR, China"      "Hungary"                  
# [28] "Ireland"                   "Israel"                    "Italy"                    
# [31] "Jamaica"                   "Japan"                     "Jordan"                   
# [34] "Kyrgyz Republic"           "Latvia"                    "Lebanon"                  
# [37] "Liechtenstein"             "Lithuania"                 "Luxembourg"               
# [40] "Malta"                     "Moldova"                   "Montserrat"               
# [43] "Nauru"                     "North Macedonia"           "Norway"                   
# [46] "Poland"                    "Portugal"                  "Romania"                  
# [49] "Serbia"                    "Sint Maarten (Dutch part)" "Slovak Republic"          
# [52] "Slovenia"                  "South Africa"              "Spain"                    
# [55] "Sri Lanka"                 "St. Lucia"                 "Sweden"                   
# [58] "Syrian Arab Republic"      "Thailand"                  "The Bahamas"              
# [61] "Trinidad and Tobago"       "Turks and Caicos Islands"  "Ukraine"                  
# [64] "United Kingdom"            "United States"             "Uzbekistan"               
# [67] "Venezuela"                 "Yemen"                     "Zimbabwe"

final_declining_emissions <- model_stats %>% filter(slope_estimate < 0, sig_alpha_.05 == 1) %>% pull(Country)

# 69 countries total

# Select Countries with Rising Total Emissions ----------------------------

# Of countries without declining total emissions, which have rising total emissions?
rising_emissions_candidates <- emissions_dt_2 %>%
  select(Country, Year, Total) %>%
  filter(!Country %in% final_declining_emissions) %>%
  group_by(Country) %>%
  mutate(
    max_total_emissions = max(Total, na.rm = T),
    # indicator for year of max emissions
    year_of_max_total_emissions_indicator = case_when(Total == max_total_emissions ~ 1, 
                                                      TRUE ~ 0)) %>%
  filter(year_of_max_total_emissions_indicator == 1) %>%
  mutate(# if multiple periods of max emissions, choose most recent
    year_of_max_total_emissions = max(Year, na.rm = T)
  ) %>%
  ungroup() %>%
  distinct(Country, year_of_max_total_emissions)

# prepare country-level slope data
rising_emissions_slope_data <- emissions_dt_2 %>%
  left_join(rising_emissions_candidates, by = 'Country') %>%
  select(Country, Year, year_of_max_total_emissions, Total) %>%
  group_by(Country) %>%
  filter(
    # remove countries omitted by year requirement from consideration
    Country %in% c(rising_emissions_candidates %>% pull(Country)),
    # Calculate slope from minimum of 1. year with max emissions OR 2. 2000
      # To assure plentiful data for countries whose max emissions year is > 2000
    Year >= case_when(year_of_max_total_emissions < 2000 ~ year_of_max_total_emissions,
                      year_of_max_total_emissions >= 2000 ~ 2000),
    # remove countries whose total emissions are 0 after 2000
    case_when(Year >= 2000 & Total != 0 ~ TRUE,
              Year >= 2000 & Total == 0 ~ FALSE)
      # The following countries are removed: 
      # 1. "Panama Canal Zone"
      # 2. "Leeward Islands"
      # 3. "Pacific Islands (Palau)"
    ) %>%
  mutate(norm_tot = scale(Total)[,1]) %>%
  ungroup()

# Create models and summaries for each country
model_summaries_2 <- rising_emissions_slope_data %>%
  group_by(Country) %>%
  nest() %>%
  reframe(
    model = map(data, .f = ~ lm(norm_tot ~ Year, data = .x)),
    summary = map(model, broom::tidy)
  )

# Unpack critical information (Country, slope estimate, p-value, significance)
model_stats_2 <- model_summaries_2 %>%
  select(Country, summary) %>%
  unnest(summary) %>%
  group_by(Country) %>%
  reframe(
    Country,
    slope_estimate = round(estimate[2], 3),
    p_value = format.pval(p.value[2], digits = 3),
    sig_alpha_.05 = case_when(p_value <= .05 ~ 1,
                              p_value > .05 ~ 0),
    positive_slope_indicator = case_when(slope_estimate > 0 ~ 1,
                                         slope_estimate <= 0 ~ 0)
  ) %>% 
  ungroup() %>%
  distinct(Country, .keep_all = T)

# Count of countries with significant slopes
count(model_stats_2 , sig_alpha_.05)
#   sig_alpha_.05     n
# 1             0    15
# 2             1   132

# What countries do not have significant slopes? 
  # What are their average emissions from 2000 onward - slope? p-value?
rising_emissions_slope_data %>% 
  right_join(model_stats_2, by = 'Country') %>% 
  group_by(Country) %>%
  filter(Year >= 2000,
         sig_alpha_.05 == 0) %>%
  reframe(avg_emissions = mean(Total),
          slope_estimate,
          p_value) %>%
  distinct()

#   Country                   avg_emissions slope_estimate p_value
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
  right_join(model_stats_2, by = 'Country') %>% 
  group_by(Country) %>%
  filter(Year >= 2000,
         sig_alpha_.05 == 1,
         positive_slope_indicator == 0) %>%
  reframe(avg_emissions = mean(Total),
          slope_estimate,
          p_value) %>%
  distinct()

#   Country          avg_emissions slope_estimate p_value      
# 1 Canada                 565.            -0.064 0.0391       
# 2 Greenland                0.602         -0.092 0.00139      
# 3 Macao SAR, China         1.43          -0.068 0.0262       
# 4 Netherlands            165.            -0.125 0.0000003    
# 5 Switzerland             41.3           -0.133 0.00000000423

# *** Will omit for now. these slopes on normalized emissions are nearly 0

# What countries have statistically significant rising emissions?
model_stats_2 %>% 
  filter(sig_alpha_.05 == 1, positive_slope_indicator ==1) %>%
  pull(Country)

# [1] "Afghanistan"             "Albania"               "Algeria"                          
# [4] "Anguilla"                "Antarctica"            "Antigua and Barbuda"              
# [7] "Argentina"               "Armenia"               "Australia"                        
# [10] "Azerbaijan"             "Bahrain"               "Bangladesh"                       
# [13] "Belize"                 "Benin"                 "Bhutan"                           
# [16] "Bolivia"                "Uruguay"               "Bosnia and Herzegovina"           
# [19] "Botswana"               "Brunei"                "Burkina Faso"                     
# [22] "Burundi"                "Cabo Verde"            "Cambodia"                         
# [25] "Cameroon"               "Chad"                  "Chile"                            
# [28] "China"                  "Colombia"              "Comoros"                          
# [31] "Congo"                  "Costa Rica"            "Côte d'Ivoire"                    
# [34] "Dem. Rep. Congo"        "Dominica"              "Dominican Republic"               
# [37] "Ecuador"                "Egypt"                 "El Salvador"                      
# [40] "Ethiopia"               "Fiji"                  "French Polynesia"                 
# [43] "Georgia"                "Ghana"                 "Grenada"                          
# [46] "Guatemala"              "Guinea"                "Guinea-Bissau"                    
# [49] "Guyana"                 "Honduras"              "Iceland"                          
# [52] "India"                  "Indonesia"             "Iran"                             
# [55] "Iraq"                   "Kazakhstan"            "Kenya"                            
# [58] "Kiribati"               "Korea"                 "Kosovo"                           
# [61] "Kuwait"                 "Lao PDR"               "Lesotho"                          
# [64] "Liberia"                "Madagascar"            "Malawi"                           
# [67] "Malaysia"               "Maldives"              "Mali"                             
# [70] "Marshall Islands"       "Mauritania"            "Mauritius"                        
# [73] "Mexico"                 "Mongolia"              "Montenegro"                       
# [76] "Morocco"                "Mozambique"            "Myanmar"                          
# [79] "Namibia"                "Nepal"                 "New Caledonia"                    
# [82] "Nicaragua"              "Niger"                 "Nigeria"                          
# [85] "Niue"                   "Oman"                  "Pakistan"                         
# [88] "Panama"                 "Papua New Guinea"      "Paraguay"                         
# [91] "Peru"                   "Philippines"           "Qatar"                            
# [94] "Russia"                 "Rwanda"                "Saint Helena"                     
# [97] "Samoa"                  "Saudi Arabia"          "Senegal"                          
# [100] "Seychelles"            "Sierra Leone"          "Solomon Islands"                  
# [103] "Somalia"               "South Sudan"           "St. Kitts and Nevis"              
# [106] "Zambia"                "Sudan"                 "Suriname"           
# [109] "São Tomé and Principe" "Taiwan"                "Tajikistan"                       
# [112] "Tanzania"              "The Gambia"            "Timor-Leste"                      
# [115] "Togo"                  "Tonga"                 "Tunisia"                          
# [118] "Turkmenistan"          "Tuvalu"                "St. Vincent and the Grenadines"                          
# [121] "Uganda"                "United Arab Emirates"  "Bonaire, Saint Eustatius and Saba"                         
# [124] "Vanuatu"               "Viet Nam"              "West Bank and Gaza"               
# [127] "Türkiye"

final_rising_emissions <- model_stats_2 %>% 
  filter(sig_alpha_.05 == 1, positive_slope_indicator ==1) %>%
  pull(Country)


# Modeling Dataframe ------------------------------------------------------

modeling_df <- emissions_dt_2 %>%
  # Removing the following variables from scope
  select(-c('Cement', 'Flaring', 'Other')) %>%
  filter(Year >= 2000) %>%
  mutate(declining_emissions_indicator = 
           case_when(Country %in% final_declining_emissions ~ 1,
                     Country %in% final_rising_emissions ~ 0,
                     TRUE ~ NA)) %>%
  group_by(Country) %>%
  # normalized total per country
  mutate(norm_tot = scale(Total)[,1]) %>%
  ungroup()

# need to combine development indicators
  # give completeness (by country?)

count(modeling_df %>% distinct(Country, .keep_all = T), declining_emissions_indicator)
#   declining_emissions_indicator     n
# 1                             0   127
# 2                             1    68
# 3                            NA    20

# Save dataframe
# fwrite(modeling_df, file = 'C:\Users\WulfN\R Projects\Emissions-Data-Analysis\Data')



# # Since you have time series data, consider deriving features that capture 
# #   temporal patterns (e.g., trends, seasonal effects). You can use techniques 
# #   like rolling averages or differences.
# # Normalize or standardize the variables if they are on different scales to 
# #   improve the regression model's performance.
# #   standardize at the country level?
# 












