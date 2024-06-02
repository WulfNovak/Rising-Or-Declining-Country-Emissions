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
mt_c02 <- fread('./datasets/Emissions/GCB2023v43_MtCO2_flat.csv') %>%
  rename(Country_Code = `ISO 3166-1 alpha-3`)

# Country Names (Cleaned) and Codes
wdi_country <- fread('./datasets/world_data/WDICountry.csv') %>% 
  select(`Country Code`, `Short Name`)

## Per Capita Emissions - Currently Unneeded
# per_capita <- fread('./datasets/Emissions/GCB2023v43_percapita_flat.csv')

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

range(mt_c02$Year)
# 1750 to 2022

### Any 'Country' level stats to not consider? 

# mt_c02 %>% distinct(Country) %>% View()
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
  left_join(wdi_country, by = c('Country_Code' = 'Country Code')) %>%
  filter(!Country %in% c('International Transport', 'Global', 'Kuwaiti Oil Fires', 
                         'Ryukyu Islands', 'Japan', 'International Shipping', 
                         'International Aviation')
  ) %>%
  bind_rows(japan_adj) %>% 
  filter(!Total == 0) %>% # Remove 0 Total emission observations
  # The following countries had 0 emissions from 2000 onward:
  # 1. Christmas Island
  # 2. Panama Canal Zone
  # 3. Leeward Islands
  # 4. Pacific Islands (Palau)
  
  mutate(Country = 
           case_when(!is.na(Country) & !is.na(`Short Name`) ~ `Short Name`, 
                     !is.na(`Short Name`) ~ `Short Name`,
                     is.na(`Short Name`) ~ Country)) %>% # Replace country name with accurate wdi country names
  select(-c('Short Name', 'UN M49'))

## Write mt_c02_adj to datafile
fwrite(mt_c02_adj, file = './datasets/Emissions/mt_c02_adj.rds')

### Analysis of Total Emissions by Country
country_mu_sd <- mt_c02_adj %>% 
  group_by(Country) %>%
  reframe(
    mean_total = mean(Total, na.rm = T),
    sd_total = sd(Total, na.rm = T)
    )

summary(country_mu_sd %>% select(-Country))
#        mean_total           sd_total       
# Min.   :   0.0056   Min.   :   0.002  
# 1st Qu.:   0.5688   1st Qu.:   0.417  
# Median :   3.8497   Median :   3.593  
# Mean   :  49.0246   Mean   :  59.364  
# 3rd Qu.:  23.6792   3rd Qu.:  24.199  
# Max.   :2246.7168   Max.   :3311.293


# World Development Indicators Prep ---------------------------------------

## Development Indicators by Year
wdi_csv <- fread('./datasets/world_data/WDICSV.csv', header = T) %>%
  filter(`Country Code` %in% mt_c02_adj$Country_Code)

# Countries/islands omitted due to lack of development indicator data
mt_c02_adj %>% 
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

reduc_emissions_countries <- mt_c02_adj %>%
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

# 89 Countries omitted due to year requirement:
mt_c02_adj %>% filter(!Country %in% c(reduc_emissions_countries %>% pull(Country))) %>% 
  distinct(Country) %>% pull()

# [1] "Afghanistan"              "Algeria"                  "Anguilla"                
# [4] "Argentina"                "Australia"                "Bahrain"                 
# [7] "Bangladesh"               "Belize"                   "Benin"                   
# [10] "Burkina Faso"             "Burundi"                  "Cambodia"                
# [13] "Chad"                     "Chile"                    "China"                   
# [16] "Colombia"                 "Comoros"                  "Congo"                   
# [19] "Dominican Republic"       "El Salvador"              "Ethiopia"                
# [22] "Fiji"                     "Panama Canal Zone"        "Gambia"                  
# [25] "Ghana"                    "Grenada"                  "Guatemala"               
# [28] "Guinea"                   "Guinea-Bissau"            "Guyana"                  
# [31] "Honduras"                 "India"                    "Indonesia"               
# [34] "Iraq"                     "Kenya"                    "Kiribati"                
# [37] "Kuwait"                   "Laos"                     "Leeward Islands"         
# [40] "Lesotho"                  "Libya"                    "Madagascar"              
# [43] "Malawi"                   "Malaysia"                 "Maldives"                
# [46] "Mali"                     "Marshall Islands"         "Mauritania"              
# [49] "Mexico"                   "Mongolia"                 "Morocco"                 
# [52] "Myanmar"                  "Nepal"                    "Nicaragua"               
# [55] "Niger"                    "Nigeria"                  "State of Palestine"      
# [58] "Oman"                     "Pacific Islands (Palau)"  "Pakistan"                
# [61] "Panama"                   "Papua New Guinea"         "Paraguay"                
# [64] "Peru"                     "Philippines"              "Bolivia"                 
# [67] "Qatar"                    "Cameroon"                 "Sudan"                   
# [70] "Rwanda"                   "Saint Helena"             "Samoa"                   
# [73] "Saudi Arabia"             "Senegal"                  "Seychelles"              
# [76] "Saint Kitts and Nevis"    "St. Kitts-Nevis-Anguilla" "Suriname"                
# [79] "Taiwan"                   "Tonga"                    "Tunisia"                 
# [82] "Türkiye"                  "Turkmenistan"             "Tuvalu"                  
# [85] "Uganda"                   "United Arab Emirates"     "Tanzania"                
# [88] "Vanuatu"                  "Viet Nam"                 "Zambia"                  
# [91] "International Aviation"

max_year_emissions <- reduc_emissions_countries %>% select(Country, year_of_max_total_emissions)

### Finding slope and statistical significance
# By country, filter to data ranging from max year to 2022
# get slope based on year (as year increases, total emissions decrease)
# indicate countries whose emissions do NOT decrease

emissions_slope_data <- mt_c02_adj %>%
  left_join(max_year_emissions, by = 'Country') %>%
  select(Country, Year, year_of_max_total_emissions, Total) %>%
  group_by(Country) %>%
  filter(# remove countries omitted by year requirement from consideration
         Country %in% c(max_year_emissions %>% pull(Country)),
         Year >= year_of_max_total_emissions) %>%
  mutate( # normalize total by year 
    norm_tot = scale(Total)[,1]) 

model_summaries <- emissions_slope_data %>%
  group_by(Country) %>%
  nest() %>%
  reframe(
    model = map(data, .f = ~ lm(norm_tot ~ Year, data = .x)),
    summary = map(model, broom::tidy)
  )

model_stats <- model_summaries %>%
  select(Country, summary) %>%
  unnest(summary) %>%
  group_by(Country) %>%
  reframe(
    Country,
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

# take the compliment of the set of countries with declining emissions
#   perhaps only select those with significant positive slope?
# 
# 
# # Compliment of declining emissions dataset
# rising_emissions <- mt_c02_adj %>%
#   select(Country, Year, Total) %>%
#   filter(Year >= 2000,
#          !Country %in% final_declining_emissions) %>%
#   group_by(Country) %>%
#   mutate( # creating indicator for positive slope
#     slope_emissions_vs_year = coef(lm(Total ~ Year))[2],
#     positive_slope_indicator = case_when(slope_emissions_vs_year >= 0 ~ 1,
#                                          slope_emissions_vs_year < 0 ~ 0)
#   ) %>%
#   ungroup() %>%
#   distinct(Country, slope_emissions_vs_year, positive_slope_indicator)
#   # distinct(Country) %>%
#   # filter(!Country %in% final_declining_emissions) %>%
#   # pull()
# 
# # Should this list be refine to countries who have a positive slope from 2000 to 2022?
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # The following dataframe 
# wdi_filtered_3 <- mt_c02_adj %>% 
#   select(Country, Country_Code, Year, Total, Coal, Oil, Gas) %>% 
#   filter(Country_Code %in% (wdi_filtered_2 %>% distinct(Country_Code) %>% pull())) %>% # min year with development indicators available
#   left_join(wdi_filtered_2 %>% mutate(Year = as.integer(Year)), 
#             by = c("Country_Code", "Year")) %>%
#   pivot_wider(names_from = 'Indicator_Name', values_from = 'value') 
#   # standardize or normalize the variables 
# 
# 
# # Since you have time series data, consider deriving features that capture 
# #   temporal patterns (e.g., trends, seasonal effects). You can use techniques 
# #   like rolling averages or differences.
# # Normalize or standardize the variables if they are on different scales to 
# #   improve the regression model's performance.
# #   standardize at the country level?
# 











