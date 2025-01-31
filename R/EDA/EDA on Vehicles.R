# loading Libraries
library(tidyverse)
library(tmap)
library(sf)
library(terra)
library(ggnewscale)
library(viridis)
library(ggtext)
library(patchwork)

# loading Data
vehicle <- read_csv("C:/My Portfolio/Best Portfolio projects/R/EDA/vehicle (2).csv")
car <- as_tibble(vehicle)
car

# Filter
car %>%
  filter(State %in% c('CA', 'TX', 'FL'))

car %>%
  filter(State == 'CA', Mileage > 20)

# Arrange
car %>%
  filter(State %in% c('CA', 'TX', 'FL')) %>%
  arrange(desc(Mileage))

# Summarise
car %>%
  summarise(
    Avg_lc = mean(lc, na.rm = TRUE),
    sd_lc = sd(lc, na.rm = TRUE),
    max_lc = max(lc, na.rm = TRUE),
    min_lc = min(lc, na.rm = TRUE),
    sum_lc = sum(lc, na.rm = TRUE),
    median_lc = median(lc, na.rm = TRUE),
    total = n()
  )

# Group by
car %>%
  group_by(State) %>%
  summarise(
    Avg_lc = mean(lc, na.rm = TRUE),
    sd_lc = sd(lc, na.rm = TRUE),
    max_lc = max(lc, na.rm = TRUE),
    min_lc = min(lc, na.rm = TRUE),
    sum_lc = sum(lc, na.rm = TRUE),
    median_lc = median(lc, na.rm = TRUE),
    total = n()
  ) %>%
  arrange(desc(Avg_lc))

# Mutate
car %>%
  group_by(State) %>%
  mutate(cph = sum(lc, na.rm = TRUE) / sum(lh, na.rm = TRUE)) %>%
  summarise(
    Avg_cph = mean(cph, na.rm = TRUE),
    Avg_mileage = mean(Mileage, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_cph))

# Visualization

# Histogram
car %>%
  filter(State %in% c('CA', 'TX', 'FL')) %>%
  ggplot(aes(x = lc, fill = State)) +
  geom_histogram(alpha = 0.8, color = 'darkblue', bins = 30) +
  labs(title = 'Labor Cost in Top 3 States', x = 'Labor Cost', y = 'Frequency') +
  facet_wrap(~State) +
  theme_minimal()

# Density Plot
car %>%
  filter(State %in% c('CA', 'TX', 'FL')) %>%
  ggplot(aes(x = lc, fill = State)) +
  geom_density(alpha = 0.5) +
  labs(title = 'Labor Cost in Top 3 States', x = 'Labor Cost') +
  theme_minimal()

# Scatter Plot
car %>%
  filter(State %in% c('CA', 'TX', 'FL')) %>%
  ggplot(aes(x = lh, y = lc, color = State, size = mc)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  facet_wrap(~State) +
  labs(title = 'Labor Cost vs Labor Hours', x = 'Labor Hours', y = 'Labor Cost') +
  theme_minimal()

# Bar Plot
new <- car %>%
  group_by(State) %>%
  mutate(cph = lc / lh) %>%
  summarise(
    Avg_cph = mean(cph, na.rm = TRUE),
    Avg_mileage = mean(Mileage, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_cph))

ggplot(new, aes(x = reorder(State, Avg_cph), y = Avg_cph, fill = State)) +
  geom_col() +
  coord_flip() +
  labs(title = 'Cost Per Hour by State', x = 'State', y = 'Avg Cost Per Hour') +
  theme_minimal()

# Box Plot
car %>%
  group_by(State) %>%
  filter(n() > 40) %>%
  ggplot(aes(x = State, y = lc, color = State)) +
  geom_boxplot() +
  labs(title = 'Labor Cost Distribution by State', x = 'State', y = 'Labor Cost') +
  theme_minimal()


