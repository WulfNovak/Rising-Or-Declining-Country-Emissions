# ##############################################################################
# Author: Wulf Novak
#
# Project: Development Indicators of Rising 
#          and Declining country Emissions
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

# read in data
emissions_dt_2 <- fread(file = './datasets/Emissions/emissions_dt_2.rds')
modeling_df <- fread(file = './datasets/Emissions/modeling_df.rds') %>% 
  select(country, country_code, year, total, declining_emissions_indicator, norm_tot,
         slope_estimate_rising, slope_estimate_declining, 
         sig_alpha_.05_rising, sig_alpha_.05_declining,
         tot_slope_estimate, per_capita_slope_estimate,
         nt_clust, clust_slope_decline) %>%
  group_by(year, declining_emissions_indicator) %>%
  # for rising/declining emissions plots
  mutate(avg_emissions_by_year = mean(total, na.rm = T), 
         norm_avg_emissions_by_year = mean(norm_tot, na.rm = T)) %>%
  ungroup()
  
# C02 Emissions Visualization ---------------------------------------------

## Df for plotting total Emissions by country Over Time
plot_df <- emissions_dt_2 %>%
  group_by(country) %>%
  mutate(tot_emissions = sum(total, na.rm = T)) %>%
  ungroup() %>%
  transmute(
    country,
    year, 
    total, 
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

## total Emissions by country Over Time
# Perhaps this is only useful as a plotly graph
ggplot(plot_df %>% filter(year >= 1900), #country_emissions_quantile == 4) , 
       aes(x = year, y = total, color = country)) +
  geom_line(show.legend = FALSE) + 
  facet_wrap(~country_emissions_quantile, scales = 'free_y') + 
  scale_y_continuous(labels = scales::comma, n.breaks = 7) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_color_viridis_d(option = 'E') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 37)) +
  labs(
    title = 'C02 Emissions by country over Time',
    subtitle = 'year 1900 Onwards',
    y = 'total C02 Emissions'
  )

# Countries that emit the most C02 generally began emitting before the 1950s

## coal, oil, gas, cement, and other as percentage of total World Emissions over time
plot_df_2 <- emissions_dt_2 %>%
  group_by(year) %>%
  transmute(
    year,
    global_total = sum(total, na.rm = T),
    global_coal = sum(coal, na.rm = T),
    global_oil = sum(oil, na.rm = T),
    global_gas = sum(gas, na.rm = T),
    global_cement = sum(cement, na.rm = T),
    global_flaring = sum(flaring, na.rm = T),
    global_other = sum(other, na.rm = T),
    coal = round(`global_coal` * 100/ `global_total`, 2),
    oil = round(`global_oil` * 100 / `global_total`, 2),
    gas = round(`global_gas` * 100 / `global_total`, 2),
    cement = round(`global_cement` * 100 / `global_total`, 2),
    flaring = round(`global_flaring` * 100 / `global_total`, 2),
    other = round(`global_other` * 100 / `global_total`, 2)
  ) %>% 
  distinct(year, .keep_all = T) %>%
  pivot_longer(
    cols = c('coal', 'oil', 'gas', 'cement', 'flaring', 'other'),
    names_to = 'Emissions Category',
    values_to = 'percent'
  ) 

## Area plot of global_emissions percent by category
plot_2 <- ggplot(plot_df_2 %>% filter(year >= 1850), 
                 aes(x = year, 
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
    x = 'year',
    y = 'Percent'
  )

# ggsave('global_percent_emissions_by_cat.svg', plot = plot_2,
#        path = './R Projects/Emissions-Data-Analysis')

# oil and gas has contributed a greater percentage to global_emissions up through
# The 1970s, but has plateaued or is decreasing since then. 

## Area plot of absolute global_emissions over time
plot_df_3 <- plot_df_2 %>%
  distinct(year, .keep_all = T) %>%
  select(-c('Emissions Category', 'percent'),
         coal = `global_coal`,
         oil = `global_oil`,
         gas = `global_gas`,
         cement = `global_cement`,
         flaring = `global_flaring`,
         other = `global_other`
  ) %>%
  pivot_longer(
    cols = c('coal', 'oil', 'gas', 'cement',
             'flaring', 'other'),
    names_to = 'Global Emissions Category',
    values_to = 'Total Emissions'
  ) 

plot_3 <- ggplot(plot_df_3 %>% filter(year >= 1850), 
                 aes(x = year, 
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
    subtitle = 'Omitting years Prior to 1850',
    x = 'Year',
    y = 'Total Emissions (Billion Tonnes)'
  )

# ggsave('global_total_emissions_by_cat.svg', plot = plot_3,
#        path = './R Projects/Emissions-Data-Analysis')

# total Emissions has steadily risen, but has arguably plateaued after 2000.
# global_gas emissions has steadily increased, despite the total emissions trend. 
# Tonnes is equivalent to 1,000 Kilograms

# Rising / Declining Emissions Visualization ------------------------------

### Rising
rise_plot_df <- modeling_df %>%
  filter(declining_emissions_indicator == 'rising')

# Countries with Rising min-max normalized C02 Emissions Average 
plot_4 <- ggplot(data = rise_plot_df %>% distinct(year, .keep_all = T), 
       aes(x = year, y = norm_avg_emissions_by_year, color = country)) +
  geom_area(show.legend = F) +
  scale_y_continuous(limits = c(0:1), n.breaks = 7) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_color_viridis_d(option = 'E') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 37)) +
  labs(
    title = 'Rising: Normalized Global Average C02 Emissions',
    x = 'Year',
    y = 'Normalized Average'
  )

ggsave('rising_normalized_emissions.svg', plot = plot_4,
       path = './R Projects/Emissions-Data-Analysis/Plots')

# All Countries
plot_5 <- ggplot(data = rise_plot_df, 
       aes(x = year, y = norm_tot, color = country)) +
  geom_line(show.legend = F) +
  scale_y_continuous(limits = c(0:1), n.breaks = 7) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_color_viridis_d(option = 'E') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 37)) +
  labs(
    title = 'Rising: Normalized Global Average C02 Emissions',
    subtitle = 'All Countries',
    x = 'Year',
    y = 'Normalized Average'
  )

ggsave('avg_rising_normalized_emissions.svg', plot = plot_5,
       path = './R Projects/Emissions-Data-Analysis/Plots')

### Declining
decline_plot_df <- modeling_df %>%
  filter(declining_emissions_indicator == 'declining')

# Countries with Declining min-max normalized C02 Emissions Average 
plot_6 <- ggplot(data = decline_plot_df %>% distinct(year, .keep_all = T), 
       aes(x = year, y = norm_avg_emissions_by_year, color = country)) +
  geom_area(show.legend = F) +
  scale_y_continuous(limits = c(0:1),  n.breaks = 7) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_color_viridis_d(option = 'E') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 37)) +
  labs(
    title = 'Declining: Normalized Global Average C02 Emissions',
    x = 'Year',
    y = 'Normalized Average'
  ) # years with economic recessions have lower emissions 

ggsave('avg_declining_normalized_emissions.svg', plot = plot_6,
       path = './R Projects/Emissions-Data-Analysis/Plots')

# All Countries
plot_7 <- ggplot(data = decline_plot_df, 
       aes(x = year, y = norm_tot, color = country)) +
  geom_line(show.legend = F) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_color_viridis_d(option = 'E') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 37)) +
  labs(
    title = 'Declining: Normalized Global Average C02 Emissions',
    x = 'Year',
    y = 'Normalized Average'
  )

ggsave('declining_normalized_emissions.svg', plot = plot_7,
       path = './R Projects/Emissions-Data-Analysis/Plots')

# Choropleth --------------------------------------------------------------

librarian::shelf(leaflet, geojsonio, htmlwidgets, gganimate, 
                 leaflet.extras2, sf, viridis, gifski)

# create dataframe for choropleth plots
mapping_df <- modeling_df %>% 
  # may engineer other features 
  select(country, id = country_code, declining_emissions_indicator,
         slope_estimate_rising, slope_estimate_declining,
         tot_slope_estimate, per_capita_slope_estimate,
         nt_clust) %>%
  mutate(trend = # slope was calculated on normalized total emissions values
           case_when(
             is.na(slope_estimate_rising) == TRUE & is.na(slope_estimate_declining) == TRUE ~ 0,
             is.na(slope_estimate_rising) == TRUE ~ slope_estimate_declining,
             is.na(slope_estimate_declining) == TRUE ~ slope_estimate_rising),
         binary_trend = case_when(trend < 0 ~ 'declining',
                                  trend > 0 ~ 'rising',
                                  TRUE ~ 'insignificant trend'),
         clust_trend = case_when(
           nt_clust == 'declining' ~ 'declining',
           nt_clust == 'non_declining' ~ 'non-declining',
           TRUE ~ NA)
  ) %>% 
  select(-starts_with('slope')) %>%
  distinct(country, .keep_all = T)

# Load GeoJSON data for countries
geojson_url <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
world_geojson <- geojson_read(geojson_url, what = "sp")

# Replace data with added variables
join_df <- world_geojson@data %>%
  left_join(mapping_df, by = 'id')
world_geojson@data <- join_df
 
# Leaflet Map Function
leaflet_map <- function(data, variable, palette, legend_title, map_save_name){
  
  map_name <- leaflet(data,
          options = leafletOptions(minZoom = 2.34)
  ) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~palette(variable),
      weight = 1,
      opacity = 1,
      color = "#655",
      fillOpacity = 0.6,
      highlight = highlightOptions(
        weight = 3,
        color = "white",
        fillOpacity = .9,
        bringToFront = TRUE
      ),
      label = ~paste(name, ": ", variable), # ,//n trend: , trend)
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(pal = palette, values = ~variable, opacity = 0.7,
              title = legend_title, position = "bottomright") %>%
    setView(lng = 0, lat = 0, zoom = 1)
  
  file <- paste0("./WulfNovak.github.io/docs/", map_save_name, '.html')
  saveWidget(map_name,
             file = file)
}

## Choropleth of trend variable
data <- world_geojson
palette <- colorBin(palette = "plasma", domain = world_geojson@data$trend, bins = 12)
legend_title <- htmltools::HTML("Min-Max<br>Normalized Slope")
variable <- world_geojson$trend
map_save_name <- 'trend_choropleth'

leaflet_map(data, variable, palette, legend_title, map_save_name)

## Binary Trend Map
# data <- world_geojson # same
palette <- colorFactor(palette = "plasma", 
                       domain = world_geojson@data$binary_trend)
legend_title <- htmltools::HTML("Total Emissions<br>Country Trend")
variable <- world_geojson$binary_trend
map_save_name <- 'binary_trend_map'

leaflet_map(data, variable, palette, legend_title, map_save_name)

# total emissions slope map
palette <- colorBin(palette = 'plasma',
                    domain = world_geojson@data$tot_slope_estimate)
legend_title <- htmltools::HTML("Absolute Total Emissions<br>Country Trend")
variable <- world_geojson$tot_slope_estimate
map_save_name <- 'tot_emissions_trend'

leaflet_map(data, variable, palette, legend_title, map_save_name)

# per capita slope map
palette <- colorBin(palette = 'plasma',
                    domain = world_geojson@data$per_capita_slope_estimate)
legend_title <- htmltools::HTML("Per Capita Emissions<br>Country Trend")
variable <- world_geojson$per_capita_slope_estimate
map_save_name <- 'per_capita_trend'

leaflet_map(data, variable, palette, legend_title, map_save_name)

# Binary rising / declining by kml clustering
palette <- colorFactor(palette = 'plasma',
                    domain = world_geojson@data$clust_trend)
legend_title <- htmltools::HTML("Total Emissions by<br>Longitudinal<br>Clustering")
variable <- world_geojson$clust_trend
map_save_name <- 'cluster_binary_trend'

leaflet_map(data, variable, palette, legend_title, map_save_name)


### For animated time series

# prepare dataset
# Load GeoJSON data for countries
geojson_url <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"

animated_map_dt <- read_sf(geojson_url) %>%
  left_join(modeling_df %>%
              select(country, id = country_code, year, norm_tot, 
                     declining_emissions_indicator, clust_slope_decline, nt_clust), 
            by = 'id')

# Color palette
# palette <- colorBin(palette = "plasma", domain = world_geojson@data$trend, bins = 12)
# legend_title <- htmltools::HTML("Min-Max<br>Normalized Slope")

map_base <- animated_map_dt %>%
  ggplot(aes(fill = norm_tot,
             group = country
             #text = str_c(id, ": ", norm_tot)
             )) + 
  geom_sf(colour = NA) + # colour = NA removes boundaries
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 12, unit = "pt")),
        legend.position = c(.1, .25)) +
  transition_time(year) + 
  # ease_aes('cubic-in-out') +
  pause_end(pause_length = 10) +
  labs(
    title = 'Normalized Country Emissions, for Year {frame_time}',
    subtitle = 'From 2000 to 2022: 1 = highest emissions, 0 = lowest emissions'
  )

animated_map <- animate(map_base, 
                        renderer = gifski_renderer('norm_tot_animation.gif'),
                        fps = 3,
                        end_pause = 9)

anim_save(filename = "./WulfNovak.github.io/docs/norm_tot_animation.gif", 
          width = 800, height = 600, res = 5000, animation = animated_map) 




