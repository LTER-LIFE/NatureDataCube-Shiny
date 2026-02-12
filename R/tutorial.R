# ============================================================
<<<<<<< HEAD
<<<<<<< HEAD
# 🐦 Adventure Tutorial: Bird Nests Meet Weather!
# ============================================================
# Today, we’re going on a mini data adventure:
# we have our own dataset about bird nests in the Veluwe, and grab weather info through
# the Data Nature Cube
<<<<<<< HEAD
# and see if temperature affects when birds lay their eggs.
#
# Along the way, you’ll:
# 1. Load and peek at our nest data
# 2. Use a Shiny app to grab weather data
# 3. Clean, filter, and merge datasets
# 4. Make a plot of lay dates vs temperature
#
# Let’s go! Run step by step (Ctrl + Enter) and have fun! 
=======
# Tutorial: Linking bird nest data with weather data
=======
# 🐦 Adventure Tutorial: Bird Nests Meet Weather!
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
# ============================================================
# Today, we’re going on a mini data adventure:
# we’ll explore bird nests, grab weather info,
=======
>>>>>>> 6f41a00 (Revise bird nest tutorial for clarity and updates)
# and see if temperature affects when birds lay their eggs.
#
<<<<<<< HEAD
# Run this script step by step using Ctrl + Enter
>>>>>>> 5bb21b5 (Add files via upload)
=======
# Along the way, you’ll:
# 1. Load and peek at our nest data
# 2. Use a Shiny app to grab weather data
# 3. Clean, filter, and merge datasets
# 4. Make a plot of lay dates vs temperature
#
# Let’s go! Run step by step (Ctrl + Enter) and have fun! 
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
# ============================================================


# ============================================================
<<<<<<< HEAD
<<<<<<< HEAD
# 1. Load libraries
# ============================================================
library(tidyverse)   
library(lubridate)  
library(dplyr)       


# ============================================================
# 2. Load our collected Bird Nest data, (supplied by Joseph Burant, PhD)
<<<<<<< HEAD
# ============================================================
# This CSV contains:
# - lay_date: when eggs were laid
# - species
# - clutch_size: number of eggs
=======
# 1. Load required libraries
=======
# 1. Load libraries
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
# ============================================================
library(tidyverse)   
library(lubridate)  
library(dplyr)       


# ============================================================
# 2. Load nest box data, supplied by Joseph Burant, PhD
=======
>>>>>>> 6f41a00 (Revise bird nest tutorial for clarity and updates)
# ============================================================
# This CSV contains:
# - lay_date: when eggs were laid
# - species
<<<<<<< HEAD
# - clutch_size (number of eggs)
>>>>>>> 5bb21b5 (Add files via upload)
=======
# - clutch_size: number of eggs
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
# - year

data <- read.csv("data/first_nests_HV.csv")

<<<<<<< HEAD
<<<<<<< HEAD
# ============================================================
# 3. Convert lay_date to a date format
# ============================================================
# Sometimes R thinks dates are just text. Let’s fix that.

data$lay_date <- as.Date(data$lay_date)

# ============================================================
# 4. Filter the nest data
# ============================================================
# Let’s keep only:
# - nests from 2024 onwards
# - nests with at least one egg
# - nests from March → July (peak breeding season)
=======

=======
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
# ============================================================
# 3. Convert lay_date to a date format
# ============================================================
# Sometimes R thinks dates are just text. Let’s fix that.

data$lay_date <- as.Date(data$lay_date)

# ============================================================
# 4. Filter the nest data
# ============================================================
<<<<<<< HEAD
 We keep only:
 - data from 2024 onwards
 - nests with at least one egg
- nests from March to July (breeding season)
>>>>>>> 5bb21b5 (Add files via upload)
=======
# Let’s keep only:
# - nests from 2024 onwards
# - nests with at least one egg
# - nests from March → July (peak breeding season)
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)

data <- data %>%
  filter(
    year >= 2024,
    clutch_size > 0,
    month(lay_date) %in% 3:7
  )

<<<<<<< HEAD
<<<<<<< HEAD
# 🐣 Mini challenge (optional): How many nests remain after filtering?
# Hint: use nrow(data)


<<<<<<< HEAD
# ============================================================
# 5. Open the Shiny app and select weather data
# ============================================================
# Time for some interactive fun! The app lets you explore different datasets,
# but we’re focusing on weather today.
#
# Once you finish, the data will be stored in `weather_data`.

weather_data <- runApp("R/naturedatacube_app/app.R")

# Shiny app steps:
# 1. Select project: Nestboxes
# 2. Zoom to Arnhem and select the polygon north of A12 ("De Hoge Veluwe")
# 3. Pick "Weather" under available datasets
# 4. Select the period: 2024-01-01 → 2025-01-01
# 5. Click "Add to overview"
# 6. Click "Return data to R" and close the app
#
# Now your weather data lives in `weather_data`! 🎉

# 🐣 Mini challenge: Explore weather_data$datasets to see what type of weather data is available


# ============================================================
# 6. Extract daily mean temperature 
# ============================================================
# We’ll select just the date and mean_temperature columns and rename them.

mean_temp <- weather_data$datasets$Weather_1 %>%
=======
=======
# 🐣 Mini challenge (optional): How many nests remain after filtering?
# Hint: use nrow(data)
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)

=======
>>>>>>> 6f41a00 (Revise bird nest tutorial for clarity and updates)
# ============================================================
# 5. Open the Shiny app and select weather data
# ============================================================
# Time for some interactive fun! The app lets you explore different datasets,
# but we’re focusing on weather today.
#
# Once you finish, the data will be stored in `weather_data`.

weather_data <- runApp("R/naturedatacube_app/app.R")

# Shiny app steps:
# 1. Select project: Nestboxes
# 2. Zoom to Arnhem and select the polygon north of A12 ("De Hoge Veluwe")
# 3. Pick "Weather" under available datasets
# 4. Select the period: 2024-01-01 → 2025-01-01
# 5. Click "Add to overview"
# 6. Click "Return data to R" and close the app
#
# Now your weather data lives in `weather_data`! 🎉

# 🐣 Mini challenge: Explore weather_data$datasets to see what type of weather data is available


# ============================================================
# 6. Extract daily mean temperature 
# ============================================================
# We’ll select just the date and mean_temperature columns and rename them.

<<<<<<< HEAD
weather <- temp$datasets$Weather_1 %>%
>>>>>>> 5bb21b5 (Add files via upload)
=======
mean_temp <- weather_data$datasets$Weather_1 %>%
>>>>>>> 6f41a00 (Revise bird nest tutorial for clarity and updates)
  select(
    date = datum,
    temp = mean_temperature
  )

<<<<<<< HEAD
<<<<<<< HEAD
# ============================================================
# 7. Make sure weather dates are Date objects
# ============================================================
mean_temp$date <- as.Date(mean_temp$date)
=======

=======
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
# ============================================================
# 7. Make sure weather dates are Date objects
# ============================================================
<<<<<<< HEAD
weather$date <- as.Date(weather$date)
>>>>>>> 5bb21b5 (Add files via upload)
=======
mean_temp$date <- as.Date(mean_temp$date)

>>>>>>> 6f41a00 (Revise bird nest tutorial for clarity and updates)

# ============================================================
# 8. Merge nest data with temperature data
# ============================================================
<<<<<<< HEAD
<<<<<<< HEAD
# Each nest now gets the mean temperature on its lay date.

plot_data <- data %>%
  left_join(mean_temp, by = c("lay_date" = "date"))
<<<<<<< HEAD
=======
We join the two datasets so that each nest record
gets the temperature from the corresponding lay date.
=======
# Each nest now gets the mean temperature on its lay date.
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)

plot_data <- data %>%
  left_join(weather, by = c("lay_date" = "date"))
>>>>>>> 5bb21b5 (Add files via upload)
=======

>>>>>>> 6f41a00 (Revise bird nest tutorial for clarity and updates)

# ============================================================
# 9. Keep only nests from 2024
# ============================================================
<<<<<<< HEAD
<<<<<<< HEAD
# This keeps the plot focused on a single breeding season.
=======
# This ensures the plot only shows data from one year.
>>>>>>> 5bb21b5 (Add files via upload)
=======
# This keeps the plot focused on a single breeding season.
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)

plot_data <- plot_data %>%
  filter(year(lay_date) == 2024)


# ============================================================
<<<<<<< HEAD
<<<<<<< HEAD
# 10. Make a plot!
# ============================================================
# What we’ll see:
# - X-axis: lay date
# - Y-axis: mean daily temperature
# - Point size: clutch size
# - Point color: species

ggplot(plot_data, aes(y = lay_date, x = temp)) +
  geom_point(
    aes(size = clutch_size, colour = species),
    alpha = 0.5
=======
# 10. Create the plot
=======
# 10. Make a plot!
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
# ============================================================
# What we’ll see:
# - X-axis: lay date
# - Y-axis: mean daily temperature
# - Point size: clutch size
# - Point color: species

ggplot(plot_data, aes(y = lay_date, x = temp)) +
  geom_point(
    aes(size = clutch_size, colour = species),
<<<<<<< HEAD
    alpha = 0.4
>>>>>>> 5bb21b5 (Add files via upload)
=======
    alpha = 0.5
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
  ) +
  scale_size_continuous(
    name = "Clutch size",
    range = c(1, 15)
  ) +
  labs(
<<<<<<< HEAD
<<<<<<< HEAD
    y = "Lay date",
    x = "Mean daily temperature (°C)",
    colour = "Species"
  ) +
  theme_minimal()

# 🎉 Congratulations! You’ve now linked bird nests with weather and
# visualized how temperature might influence egg-laying. 


# 🐣 Optional exploration challenge:
# - Can you add a smooth trend line to see the temperature effect?
=======
    x = "Lay date",
    y = "Mean daily temperature (°C)",
=======
    y = "Lay date",
    x = "Mean daily temperature (°C)",
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
    colour = "Species"
  ) +
  theme_minimal()
<<<<<<< HEAD
>>>>>>> 5bb21b5 (Add files via upload)
=======

<<<<<<< HEAD
>>>>>>> f4f57e4 (Revise tutorial for linking bird nest and weather data)
=======
# 🎉 Congratulations! You’ve now linked bird nests with weather and
# visualized how temperature might influence egg-laying. 


# 🐣 Optional exploration challenge:
# - Can you add a smooth trend line to see the temperature effect?
>>>>>>> 5c07bc0 (Revise bird nest and weather tutorial for clarity)
