# ##############################################################################
# Author: Wulf Novak
#
# Project: 
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



# Goal: Select indicator variables with high global data coverage
#
# Steps to acheive goal
# 1. Assess missing data globally, and by country
#   columns = var, max global missing, max country missing
# 2. Limit to < 25% NAs allowed globally, and x% max per country.
# 3. See what variables fit that criteria.
# 4. Select subset for emissions modeling.
# 5. Transform and impute remaining values by country. 


# Setup -------------------------------------------------------------------

source('./R Projects/Emissions-Data-Analysis/Setup.R')

# Write emissions_dt_2 to datafile
emissions_dt_2 <- fread(file = './datasets/Emissions/emissions_dt_2.rds')

wdi_csv <- fread('./datasets/world_data/WDICSV.csv', header = T) %>%
  filter(`Country Code` %in% emissions_dt_2$country_code) %>%
  clean_names()
### Categorized data
# na_problem <- fread('./R Projects/Emissions-Data-Analysis/Data/Development_Indicators_Categorized.csv')



# Variable Selection Based on Completeness --------------------------------

# Prior iterations of variable selection have failed due to lack of 
# data available across all countries from 2000 forward. Refining list
# of 1492 indicators based on completeness first

perc_missing <- wdi_csv %>%
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
  ungroup() %>%
  filter(global_perc_na > 25)

# distn of percent missing across all countries
distn_perc_missing <- perc_missing %>% 
  select(-c('global_perc_na', 'year')) %>% 
  distinct(country_name, indicator_name, .keep_all = T) %>%
  group_by(indicator_name) %>%
  reframe(
    min_perc_na = min(perc_na),
    mean_perc_na = mean(perc_na),
    max_perc_na = max(perc_na),
    sd_perc_na = sd(perc_na),
    # vars are not likely normally dist'd, using imprecise method
    upper_bound_perc_na = 1.645 * sd_perc_na + mean_perc_na,
    num_countries_all_na = sum(perc_na == 100)
  ) %>%
  filter(upper_bound_perc_na < 80,
         mean_perc_na <= 50)

# hist of number of countries with 100% NA for indicators
ggplot(distn_perc_missing, aes(x = num_countries_all_na)) + 
  geom_histogram()
  # ** Only 4 indicators have some values for all countries

# There are always countries missing all of the data for a given variable
# impute by clustering on countries with similar values in other variables?
# pool values by year of variable for that cluster


# Cluster on emissions data OR GDP to classify countries by their output

# don't want to cluster on trend
# min, mean, max, sd cluster? 
country_emissions_stats <- emissions_dt_2 %>%
  #mutate(year = as.numeric(year)) %>%
  select(country, year, total, per_capita) %>%
  filter(year >= 2000) %>%
  group_by(country) %>%
  mutate(
    tot_mean = mean(total),
    tot_sd = sd(total),
    tot_min = min(total),
    tot_max = max(total),
    tot_range = tot_max - tot_min, 
    # Consider per capita also
    per_capita_mean = mean(per_capita),
    per_capita_sd = sd(per_capita),
    per_capita_min = min(per_capita),
    per_capita_max = max(per_capita),
    per_capita_range = per_capita_max - per_capita_min
  ) %>%
  ungroup() %>%
  mutate(across(starts_with(c('tot_', 'per_capita_')),
                .f = ~scale(.)[ ,1])) %>%
  distinct(country, .keep_all = T)

# scale dataframe
  # select number of clusters

# screeplot 

cluster_range <- 2:7 # max number of clusters possible through trial and error

kmeans_df <- tibble(
  clusters = cluster_range,
  kmeans_objects = map(cluster_range, 
        ~ kmeans(x = country_emissions_stats %>% 
                   select(starts_with('tot')) %>%
                   as.matrix(),
                 centers = ., nstart = 10))
  )


kmeans_df_1 <- country_emissions_stats %>% 
  select(starts_with('tot'))

kmeans_object_1 <- kmeans(x = as.matrix(kmeans_df_1), centers = 5, nstart = 10)
kmeans_object_1$withinss
kmeans_object_1$tot.withinss
kmeans_object_1$size

# cluster with different variables
# get the anova scores of the clusters
# find those that give the most significant difference of the groupings?
# cluster statistics, visualize


# should I include the coal, oil, and gas? (similar energy usage type)


# Cluster visualization (using k-means)

librarian::shelf(factoextra)

fviz_cluster(cluster-object, data = ----,
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )

# then per variable pooled values per cluster


### Clustering methods that will take into account the trend of the data, 
# and can be used for grouping.
# (GBTM), growth mixture modeling (GMM), and longitudinal k-means (KML)










# indicator name dist'n of NAs
# mean, max, 95% CI
  
  # dist'n of NAs for each country











