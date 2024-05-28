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
# - Visualization Section
# - Etc
# ##############################################################################


# Setup -------------------------------------------------------------------

source('./R Projects/Emissions-Data-Analysis/Setup.R')

mt_c02_adj <- fread(file = './datasets/Emissions/mt_c02_adj.rds')

# Indicator variables, model results


# Rising Emission Visualizatoin -------------------------------------------

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
    # Calculate and Create variables for quantiles
    upper_3rd = quantile(tot_emissions, prob = .75, na.rm = T) %>% unname(),
    upper_2nd = quantile(tot_emissions, prob = .5, na.rm = T) %>% unname(),
    upper_1st = quantile(tot_emissions, prob = .25, na.rm = T) %>% unname(),
    country_emissions_quantile = case_when(
      tot_emissions <= upper_1st ~ 1,
      tot_emissions <= upper_2nd ~ 2,
      tot_emissions <= upper_3rd ~ 3,
      tot_emissions > upper_3rd ~ 4,
      TRUE ~ NA)
  ) %>%
  select(-c(starts_with('upper'), tot_emissions))

## Total Emissions by Country Over Time
# Perhaps this is only useful as a plotly graph
ggplot(plot_df %>% filter(Year >= 1900), #country_emissions_quantile == 4) , 
       aes(x = Year, y = Total, color = Country)) +
  geom_line(show.legend = FALSE) + 
  facet_wrap(~country_emissions_quantile,scales = 'free_y') + 
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



# Declining Emissions Visualizations --------------------------------------

# Definition: Have 3 consecutive years where emissions are less than the max

reduc_emissions_countries <- mt_c02_adj %>%
  group_by(Country) %>%
  mutate(
    max_total_emissions = max(Total, na.rm = T),
    # indicator for year of max emissions
    year_of_max_total_emissions = case_when(Total == max_total_emissions ~ 1, 
                                            TRUE ~ 0)
  ) %>%
  filter(
    year_of_max_total_emissions == 1,
    # remove countries whose max emitting year is within past 3 years,
    Year < max(Year, na.rm = T) - 3 
  ) %>%
  distinct(Country, .keep_all = T)

reduc_emissions_countries$Year %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1949    1988    2007    2001    2014    2018 

# What is the min usage after the max year emissions - what is the percent difference?

# year and country 
max_year_emissions <- reduc_emissions_countries %>% select(Country, max_year_for_filter = Year)

min_after_max_emissions <- mt_c02_adj %>%
  left_join(max_year_emissions, by = 'Country') %>%
  group_by(Country) %>%
  filter(Year >= max_year_for_filter) %>%
  mutate(
    min_emissions_by_country = min(Total, na.rm = T),
    min_emission_year = case_when(Total == min_emissions_by_country ~ 1, 
                                  TRUE ~ 0),
    max_emission_year = min(Year, na.rm = T), 
    years_from_max_to_min = max_emission_year - min_emission_year,
    slope_emissions_v_year = coef(lm(Total ~ Year))[2]
  ) %>%
  # filter countries with a positive slope for emissions over time after their max year
  # so countries that have reduced emissions over time remain 
  filter(slope_emissions_v_year <= 0) %>%
  ungroup() 

nrow(min_after_max_emissions %>% distinct(Country))
# 116 countries with reduced emissions (131 prior)

reduc_emissions_countries_names <- min_after_max_emissions %>%
  distinct(Country) %>%
  pull()

### Plot dataframe
plot_reduc_emissions_countries <- plot_df %>%
  filter(Country %in% reduc_emissions_countries_names)

# Country emissions quantile for countries
count(plot_reduc_emissions_countries %>% distinct(Country, .keep_all = T), country_emissions_quantile)
#   country_emissions_quantile     n
# 1                          1    43
# 2                          2    24
# 3                          3    25
# 4                          4    24

# Countries with declining emissions 
ggplot(plot_reduc_emissions_countries %>% filter(Year >= 1900), 
       aes(x = Year, y = Total, color = Country)) +
  geom_line(show.legend = FALSE) + 
  facet_wrap(~country_emissions_quantile,scales = 'free_y') + 
  scale_y_continuous(labels = scales::comma, n.breaks = 7) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_color_viridis_d(option = 'E') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 37)) +
  labs(
    title = 'Countries with Declining Emissions',
    subtitle = 'Year 1900 Onwards, by Quartile',
    y = 'Total C02 Emissions'
  )

# Countries with rising emissions
plot_rising_emissions <- plot_df %>% 
  filter(!Country %in% reduc_emissions_countries_names) 

ggplot(plot_rising_emissions %>% filter(Year >= 1900), 
       aes(x = Year, y = Total, color = Country)) +
  geom_line(show.legend = FALSE) + 
  facet_wrap(~country_emissions_quantile,scales = 'free_y') + 
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
