# ##############################################################################
# Author: Wulf Novak
#
# Project: Development Indicators of Rising 
#          and Declining country Emissions
#
# Date: 2024-05-03
#
# R version 4.4.0 (2024-04-24 ucrt)
# ------------------------------------------------------------------------------
# Sections:
# - Emissions Data Prep
# - World Development Indicators Prep
# - Select Countries with Declining total Emissions
# - Select Countries with Rising total Emissions
# - Modeling Dataframe 
# ##############################################################################

# Data: World Emissions
#       https://zenodo.org/records/7215364
#       Convened by: Robbie M. Andrew and Glen Peters
#
#       World Development Indicators (wdi)
#       https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators

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

## per_capita Emissions - Currently Unneeded
# per_capita <- fread('./datasets/Emissions/GCB2023v43_percapita_flat.csv')

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

### Any 'country' level stats to not consider? 

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
fwrite(emissions_dt_2, file = './datasets/Emissions/emissions_dt_2.rds')

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

# World Development Indicators Prep ---------------------------------------

## Development Indicators by year
wdi_csv <- fread('./datasets/world_data/WDICSV.csv', header = T) %>%
  filter(`Country Code` %in% emissions_dt_2$country_code) %>%
  clean_names()

# Countries/islands omitted due to lack of development indicator data
emissions_dt_2 %>% 
  filter(!country_code %in% wdi_country$`country_code`,
         year >= 2000) %>% #
  group_by(country) %>%
  mutate(mean_total_emissions = round(mean(total, na.rm = T), 2)) %>%
  ungroup() %>%
  distinct(country, mean_total_emissions)

# country                           mean_total_emissions
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
all_ind_filt <- bind_rows(pub_ss, gov_iq, env_sus, inc_pov, econ_pg) %>%
  filter(!grepl('CPIA', # known low coverage for these variables 
                ignore.case = T, indicator))

# Saving to Reference later
fwrite(all_ind_filt, file = './R Projects/Emissions-Data-Analysis/Data/Filtered_Indicators_Categorized.csv')

# Filtering longitudinal indicator data 
wdi_filtered <- wdi_csv %>% 
  filter(indicator_name %in% all_ind_filt$indicator)

missing_vals(wdi_filtered)
# Coverage improves drastically from 2000 onward. 
# Will filter to indicators from year 2000 forward.

# Save space
rm(wdi_csv)
gc()


wdi_filtered_2 <- wdi_filtered %>% 
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
  ungroup() 
  
# How complete should the development indicators be across countries?? 
indicator_global_perc_na <- wdi_filtered_2 %>% 
  distinct(indicator_name, global_perc_na) %>% 
  arrange(desc(global_perc_na))

# 
summary(indicator_global_perc_na$global_perc_na)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02    8.89   24.15   31.03   52.96   87.30

# dist'n of data coverage
ggplot(indicator_global_perc_na, aes(x = global_perc_na)) +
  geom_density() 

# Will remove variables with < 80% data cover from 2000 onward

# How complete should development indicators be by country?
indicator_country_perc_na <- wdi_filtered_2 %>% 
  distinct(country_name, indicator_name, perc_na) %>% 
  arrange(desc(perc_na))

summary(indicator_country_perc_na$perc_na)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    4.35   31.03   69.57  100.00 

# dist'n of data coverage
ggplot(indicator_country_perc_na, aes(x = perc_na)) +
  geom_density() 
# most countries have coverage

missing_vals(wdi_filtered_2)

rm(wdi_filtered)
gc()

# Pivot indicator variables to (country, year, Indicator_1, Indicator_2, etc,) form
wdi_filtered_3 <- suppressWarnings({wdi_filtered_2 %>%
  # Removing variables missing 80% or more of data
  filter(!global_perc_na >= 80) %>%
  select(-c('perc_na', 'global_perc_na')) %>%
  group_by(country_name) %>%
  pivot_wider(names_from = 'indicator_name', values_from = 'value') %>%
  clean_names() %>%
  # Variables with the following key words have large absolute value numbers that 
  # vary greatly from other variables which typically give a percent, index, percentile.
  # Normalizing for each country
  mutate(
    across( 
      .cols = names(.)[grep("income_constant|income_current|capita_constant|capita_current|metric_tons|constant_LCU|international|kWh|current_US|cubic_meters", 
                            names(.), ignore.case = TRUE)],
      .fns = ~case_when(
        # min(NA) and max(NA) give 'inf', changing to zero
        # to handle variables where all observations are NA
        is.infinite(min(., na.rm = T)) == TRUE ~ 0, # ** Can ignore warnings
        is.infinite(max(., na.rm = T)) == TRUE ~ 0, # ** Can ignore warnings
        # case when min = max = 0 to avoid 0/0 = NaN
        (min(., na.rm = T) == 0 & max(., na.rm = T) == 0) ~ 0,
        # if min = max = value, then NA (otherwise it's NaN)
        (min(., na.rm = T) == max(., na.rm = T)) ~ NA,
        # (value - min(value) / max(value) - min(value)) * 100
        TRUE ~ (.-min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)) * 100),
      .names = "norm_{col}" 
    )
  ) %>%
    # Removing variables where the normalized version now exists
    select( # There had to be a better way!
      -c(
      "adjusted_net_national_income_constant_2015_us",
      "adjusted_net_national_income_current_us",                                 
      "adjusted_net_national_income_per_capita_constant_2015_us",                       
      "adjusted_net_national_income_per_capita_current_us",                             
      "aquaculture_production_metric_tons",                                      
      "capture_fisheries_production_metric_tons",                                       
      "cereal_production_metric_tons",
      "debt_service_on_external_debt_public_and_publicly_guaranteed_ppg_tds_current_us",
      "discrepancy_in_expenditure_estimate_of_gdp_constant_lcu",                  
      "exports_of_goods_services_and_primary_income_bo_p_current_us",        
      "external_debt_stocks_public_and_publicly_guaranteed_ppg_dod_current_us",
      "gdp_constant_lcu",                                               
      "gdp_per_capita_constant_2015_us",                                                
      "gdp_per_capita_constant_lcu",                                
      "gdp_per_capita_ppp_constant_2017_international",
      "gdp_ppp_constant_2017_international",                                            
      "gross_domestic_income_constant_lcu",                  
      "imports_of_goods_services_and_primary_income_bo_p_current_us",
      "net_bilateral_aid_flows_from_dac_donors_european_union_institutions_current_us",
      "net_primary_income_bo_p_current_us",                        
      "net_primary_income_net_income_from_abroad_current_us",                           
      "ppp_conversion_factor_gdp_lcu_per_international",                                
      "primary_income_payments_bo_p_current_us",                                        
      "primary_income_receipts_bo_p_current_us",                   
      "renewable_internal_freshwater_resources_per_capita_cubic_meters",            
      "renewable_internal_freshwater_resources_total_billion_cubic_meters",             
      "total_fisheries_production_metric_tons"
      )
    ) %>%
    mutate(across(.cols = -c('country_code', 'year'),
                  .fns = ~ lag(.x, n = 1), 
                  .names = "lag_{col}")) %>%
  ungroup() %>%
  mutate(year = as.integer(year)) 
})

missing_vals(wdi_filtered_3)
# Perhaps remove variables with 70% or more NA?

# Method for selecting highly correlated variables:
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


# Select Countries with Declining total Emissions -------------------------

# Declining Emissions Definition: 
# 1. Max year of emissions cannot be 2019 onward 
#    (so that there is enough data to recognize a trend)
# 2. Slope of normalized total emissions by country to year is negative and 
#    statistically significant, for data of max year of emissions onward

reduc_emissions_countries <- emissions_dt_2 %>%
  select(country, year, total) %>%
  group_by(country) %>%
  mutate(
    max_total_emissions = max(total, na.rm = T),
    # indicator for year of max emissions
    year_of_max_total_emissions_indicator = case_when(total == max_total_emissions ~ 1, 
                                            TRUE ~ 0)
  ) %>%
  filter(
    year_of_max_total_emissions_indicator == 1,
    # remove countries whose max emitting year is within past 3 years,
    year < max(year, na.rm = T) - 3 # *** Omits 91 countries
  ) %>%
  mutate(# if multiple periods of emission decline, choose most recent
         year_of_max_total_emissions = max(year, na.rm = T)
         ) %>%
  ungroup() %>%
  distinct(country, year_of_max_total_emissions, .keep_all = T)

# 87 Countries omitted due to year requirement:
emissions_dt_2 %>% filter(!country %in% c(reduc_emissions_countries %>% pull(country))) %>% 
  distinct(country) %>% pull()

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

max_year_emissions <- reduc_emissions_countries %>% select(country, year_of_max_total_emissions)

### Finding slope and statistical significance
# By country, filter to data ranging from max year to 2022
# get slope based on year (as year increases, total emissions decrease)
# indicate countries whose emissions do NOT decrease

# min-max normalize data so that slopes and values can later be compared 
# between countries more easily
min_max_normalize <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

# Prepare data from year of max total emissions onward
emissions_slope_data <- emissions_dt_2 %>%
  left_join(max_year_emissions, by = 'country') %>%
  select(country, year, year_of_max_total_emissions, total) %>%
  group_by(country) %>%
  filter(# remove countries omitted by year requirement from consideration
         country %in% c(max_year_emissions %>% pull(country)),
         year >= year_of_max_total_emissions) %>%
  mutate(norm_tot = min_max_normalize(total)) # min-max normalized of total by year 

# Create models and summaries for each country
model_summaries <- emissions_slope_data %>%
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

# Count of countries with a statistically significant slope estimate: 72
count(model_stats, sig_alpha_.05)
#   sig_alpha_.05     n
# 1             0    58
# 2             1    72

# Countries omitted due to insignificant slope OR positive slope
model_stats %>% filter(slope_estimate > 0 | sig_alpha_.05 == 0) %>% pull(country)

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
model_stats %>% filter(slope_estimate < 0, sig_alpha_.05 == 1) %>% pull(country)
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

final_declining_emissions <- model_stats %>% 
  filter(slope_estimate < 0, sig_alpha_.05 == 1) %>%
  select(country, 
         slope_estimate_declining = slope_estimate, 
         sig_alpha_.05_declining = sig_alpha_.05)

# 69 countries total with declining emissions

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

# Create models and summaries for each country
model_summaries_2 <- rising_emissions_slope_data %>%
  group_by(country) %>%
  nest() %>%
  reframe(
    model = map(data, .f = ~ lm(norm_tot ~ year, data = .x)),
    summary = map(model, broom::tidy)
  )

# Unpack critical information (country, slope estimate, p-value, significance)
model_stats_2 <- model_summaries_2 %>%
  select(country, summary) %>%
  unnest(summary) %>%
  group_by(country) %>%
  reframe(
    country,
    slope_estimate = round(estimate[2], 3),
    p_value = format.pval(p.value[2], digits = 3),
    sig_alpha_.05 = case_when(p_value <= .05 ~ 1,
                              p_value > .05 ~ 0),
    positive_slope_indicator = case_when(slope_estimate > 0 ~ 1,
                                         slope_estimate <= 0 ~ 0)
  ) %>% 
  ungroup() %>%
  distinct(country, .keep_all = T)

# Count of countries with significant slopes
count(model_stats_2 , sig_alpha_.05)
#   sig_alpha_.05     n
# 1             0    15
# 2             1   132

# What countries do not have significant slopes? 
  # What are their average emissions from 2000 onward - slope? p-value?
rising_emissions_slope_data %>% 
  right_join(model_stats_2, by = 'country') %>% 
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
  right_join(model_stats_2, by = 'country') %>% 
  group_by(country) %>%
  filter(year >= 2000,
         sig_alpha_.05 == 1,
         positive_slope_indicator == 0) %>%
  reframe(avg_emissions = mean(total),
          slope_estimate,
          p_value) %>%
  distinct()

#   country          avg_emissions slope_estimate p_value      
# 1 Canada                 565.            -0.064 0.0391       
# 2 Greenland                0.602         -0.092 0.00139      
# 3 Macao SAR, China         1.43          -0.068 0.0262       
# 4 Netherlands            165.            -0.125 0.0000003    
# 5 Switzerland             41.3           -0.133 0.00000000423

# *** Will omit for now. these slopes on normalized emissions are nearly 0

# What countries have statistically significant rising emissions?
model_stats_2 %>% 
  filter(sig_alpha_.05 == 1, positive_slope_indicator ==1) %>%
  pull(country)

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
  select(country, 
         slope_estimate_rising = slope_estimate, 
         sig_alpha_.05_rising = sig_alpha_.05)

# Modeling Dataframe ------------------------------------------------------

# Combine emissions and indicator data
modeling_df <- emissions_dt_2 %>%
  # Removing the following variables from scope
  select(-c('cement', 'flaring', 'other')) %>%
  filter(year >= 2000) %>%
  mutate(declining_emissions_indicator = 
           case_when(country %in% final_declining_emissions$country ~ 'declining',
                     country %in% final_rising_emissions$country ~ 'rising',
                     TRUE ~ 'insignificant_trend')) %>%
  group_by(country) %>%
  # min-max normalization of total by country
  left_join(final_rising_emissions, by = 'country') %>%
  left_join(final_declining_emissions, by = 'country') %>%
  mutate(norm_tot = min_max_normalize(total)) %>%
  ungroup() %>%
  left_join(wdi_filtered_3, by = c('country_code', 'year')) %>%
  filter(
    # If country name from wdi dataset is NA, there is no indicator data
    !is.na(country_name) 
    # Removed the following Countries: Also referenced in the wdi prep section
    # [1] "Anguilla"                          "Antarctica"                       
    # [3] "Bonaire, Saint Eustatius and Saba" "Cook Islands"                     
    # [5] "Kosovo"                            "Montserrat"                       
    # [7] "Niue"                              "Saint Helena"                     
    # [9] "Saint Pierre and Miquelon"         "Taiwan"                           
    # [11] "Wallis and Futuna Islands"
    ) %>%
  select(-country_name) # Duplicate of 'country'
  
# Countries without development data

# The following countries are omitted by the Join:
# [1] "Bahamas, The"              "Brunei Darussalam"         "Congo, Dem. Rep."         
# [4] "Congo, Rep."               "Cote d'Ivoire"             "Curacao"                  
# [7] "Egypt, Arab Rep."          "Gambia, The"               "Iran, Islamic Rep."       
# [10] "Korea, Dem. People's Rep." "Korea, Rep."               "Micronesia, Fed. Sts."    
# [13] "Russian Federation"        "Sao Tome and Principe"     "Turkiye"                  
# [16] "Venezuela, RB"             "Yemen, Rep." 


count(modeling_df %>% distinct(country, .keep_all = T), declining_emissions_indicator)
#   declining_emissions_indicator     n
# 1 declining                        66 ***
# 2 insignificant_trend              18
# 3 rising                          120 

# *** There are only 66 countries of the 69 found with declining emissions. 
# The removed countries are 'Christmas Island', 'Cook Islands', and 'Montserrat'
# All were removed due to lack of development indicator data from 2000 onward

# Save modeling_df
fwrite(modeling_df, file = './datasets/Emissions/modeling_df.rds')


### avg Correlations of variables to emissions across countries?
### Score of average correlation by sum of data available for that variable

# Variables that are very different between groups




# # Since you have time series data, consider deriving features that capture 
# #   temporal patterns (e.g., trends, seasonal effects). You can use techniques 
# #   like rolling averages or differences.
# # Normalize or standardize the variables if they are on different scales to 
# #   improve the regression model's performance.
# #   standardize at the country level?
# 












