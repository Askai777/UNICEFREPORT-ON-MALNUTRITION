# Load the required libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(lwgeom)
library(dplyr)

# Import the unicef_indicator_2.csv file
data <- read_csv("unicef_indicator_2.csv")

# Clean and transform the data
clean_data <- data %>%
  drop_na() %>%
  filter(!is.na(obs_value) & obs_value >= 0)

# Filter for the top 20 countries with the highest obs_value
top_20 <- clean_data %>%
  group_by(country) %>%
  summarise(total_obs_value = sum(obs_value)) %>%
  top_n(20, total_obs_value)

# Load World Map dataset and make the geometry valid
world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()

# Create a lookup table to map country names to ISO A3 codes and join with top_20
top_20 <- top_20 %>%
  left_join(world_map %>% select(iso_a3, name_long) %>% distinct(), by = c("country" = "name_long"))

# Merge world_map and top_20 data frames
merged_data <- left_join(world_map, top_20, by = "iso_a3")

# Plot 20 countries with the highest obs_value
world_map_chart <- ggplot() +
  geom_sf(data = merged_data, aes(fill = total_obs_value), color = "white") +
  scale_fill_continuous(low = "blue", high = "red", na.value = "lightgreen", name = "Obs Value") +
  labs(title = "Top 20 Countries with the Highest Level of Malnutrition (Wasted, Overweight or Stunted)") +
  theme_minimal()

#View the Map 
world_map_chart


unicef_metadata <- read.csv("unicef_metadata.csv")
data_selected <- unicef_metadata %>%
  select(country, Population..total, Life.expectancy.at.birth..total..years. )

# Load the clean data and add a row number column

clean_data$id <- 1:nrow(clean_data)

# Join the two datasets on the row number column
data_combined <- left_join(data_selected, clean_data, by = "country",relationship = "many-to-many")

# Create the scatter plot with linear regression line
ggplot(data_combined, aes(x = Life.expectancy.at.birth..total..years., y = Population..total, color = obs_value )) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "green") +
  ggtitle("Scatter Plot of Population vs Life Expectancy by Observed Value for Country") +
  xlab("Life Expectancy at birth") +
  ylab("Population Total") +
  theme_minimal()


# Filter the data to the desired time periods for Countries 
clean_data_combined1 <- data_combined %>%
  filter(country %in% c( "Armenia", "Albania", "Algeria", "Bangladesh", "Bolivia","India"))

# Create the time series chart with facets for time period
ggplot(clean_data_combined1, aes(x = time_period  , y = obs_value  , group = Population..total , color = "Red")) +
  geom_line() +
  facet_wrap(~ country, ncol = 2) +
  ggtitle("Time Series Chart of Observed Value for Country by Time Period") +
  xlab("X Axis with Time Period ") +
  ylab("Y Axis with Obs-Value of Indicator") +
  theme_bw()

# Bar chart: Countries with the lowest  mean prevalence of Malnutrition
bar_chart <- ggplot(bottom_15, aes(x = reorder(country, total_obs_value), y = total_obs_value)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Countries with the Lowest Mean Prevalence of Malnutrition",
       x = "Country",
       y = "Malnutrition Prevalence (%)") +
  theme_classic()
bar_chart
