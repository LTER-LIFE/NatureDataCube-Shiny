library(dplyr)
library(tidyverse)

# ============================================================
# Load libraries
# ============================================================
library(tidyverse)
library(lubridate)

# ============================================================
# Load and clean nest data
# ============================================================
data <- read.csv("data/first_nests_HV.csv")

temp <- runApp("R/naturedatacube_app/app.R")


# ensure lay_date is Date
data$lay_date <- as.Date(data$lay_date)

# filter data
data <- data %>%
  filter(
    year >= 2024,
    clutch_size > 0,
    month(lay_date) %in% 3:7   # March–July
  )

# ============================================================
# Extract and clean weather data
# ============================================================
weather <- temp$datasets$Weather_1 %>%
  select(
    date = datum,
    temp = mean_temperature
  )

weather$date <- as.Date(weather$date)

# ============================================================
# Merge temperature onto nest data
# ============================================================
plot_data <- data %>%
  left_join(weather, by = c("lay_date" = "date"))

library(lubridate)

plot_data <- plot_data %>%
  filter(year(lay_date) == 2024)


# ============================================================
# Plot
# ============================================================
ggplot(plot_data, aes(x = lay_date, y = temp)) +
  geom_point(
    aes(size = clutch_size, colour = species),
    alpha = 0.4
  ) +
  scale_size_continuous(
    name = "Clutch size",
    range = c(1, 15)
  ) +
  labs(
    x = "Lay date",
    y = "Mean daily temperature (°C)",
    colour = "Species"
  ) +
  theme_minimal()







