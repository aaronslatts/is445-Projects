# Load lib
library(tidyverse)
library(lubridate)
library(sf)
library(viridis)
library(stringi)

# Load ds
covid_data <- read_csv("C:/Users/aaron/desktop/IS445/US_counties_COVID19_health_weather.csv")

covid_data$date <- mdy(covid_data$date)

# Remove duplicates
covid_data_cleaned <- covid_data %>%
  group_by(county, state, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")

# Filter data for the year 2020
covid_2020_cleaned <- covid_data_cleaned %>%
  filter(year(date) == 2020)
county_cases <- covid_2020_cleaned %>%
  group_by(county, state) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")

# Load json
county_geojson_url <- "https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_500k.json"
county_gdf <- st_read(county_geojson_url)

# Clean
county_gdf$NAME <- stri_trans_tolower(county_gdf$NAME)
county_cases$county <- stri_trans_tolower(county_cases$county)

# Merge
county_gdf <- county_gdf %>%
  left_join(county_cases, by = c("NAME" = "county"))

#missing data
county_gdf$total_cases <- replace_na(county_gdf$total_cases, 0)

# Cap total cases at 500k for visualization
county_gdf <- county_gdf %>%
  mutate(total_cases_capped = ifelse(total_cases > 500000, 500000, total_cases))

# Filter out non-continental US counties (Alaska, Hawaii and Puerto Rico)
continental_counties <- county_gdf %>%
  filter(!STATE %in% c("15", "72"))
continental_counties <- continental_counties %>%
  mutate(total_cases_log = log10(total_cases_capped + 1))
ggplot() +
  geom_sf(data = continental_counties, aes(fill = total_cases_log), color = "white", size = 0.05) +
  scale_fill_viridis_c(
    option = "plasma", 
    name = "Total COVID-19 Cases", 
    limits = c(0, log10(500001)),
    breaks = log10(c(1, 10, 100, 1000, 10000, 100000, 500000)),
    labels = c("1", "10", "100", "1k", "10k", "100k", "500k")
  ) +
  labs(
    title = "Total COVID-19 Cases By County 2020", 
    subtitle = "Continental United States Only"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 15, 
      barheight = 0.5, 
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50))  # Adjust limits