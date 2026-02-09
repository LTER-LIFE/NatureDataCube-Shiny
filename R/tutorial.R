# ============================================================
<<<<<<< HEAD
# 🐦 Adventure Tutorial: Bird Nests Meet Weather!
# ============================================================
# Today, we’re going on a mini data adventure:
# we have our own dataset about bird nests in the Veluwe, and grab weather info through
# the Data Nature Cube
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
# ============================================================
# This tutorial shows how to:
# 1. Load nest data
# 2. Open a Shiny app to access weather data
# 3. Clean and filter the data
# 4. Merge nest data with temperature data
# 5. Create a plot showing lay dates vs temperature
#
# Run this script step by step using Ctrl + Enter
>>>>>>> 5bb21b5 (Add files via upload)
# ============================================================


# ============================================================
<<<<<<< HEAD
# 1. Load libraries
# ============================================================
library(tidyverse)   
library(lubridate)  
library(dplyr)       


# ============================================================
# 2. Load our collected Bird Nest data, (supplied by Joseph Burant, PhD)
# ============================================================
# This CSV contains:
# - lay_date: when eggs were laid
# - species
# - clutch_size: number of eggs
=======
# 1. Load required libraries
# ============================================================
# These packages help us work with data, dates, and plots

library(tidyverse)   # data manipulation + plotting
library(lubridate)   # working with dates
library(dplyr)       # data filtering and joining


# ============================================================
# 2. Load the nest data (supplied by Joseph B. Burant, PhD)
# ============================================================
# This file contains information on bird nests such as:
# - lay_date (date eggs were laid)
# - species
# - clutch_size (number of eggs)
>>>>>>> 5bb21b5 (Add files via upload)
# - year

data <- read.csv("data/first_nests_HV.csv")

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

# ============================================================
# 3. Open the Shiny app
# ============================================================
This will open a Shiny app in a new window.
The app allows you to explore and extract all types of data 
but for now we focus on weather data.
Once the app is closed or finished, the data will be
available in the object called `temp`.

step 1. Run temp 

temp <- runApp("R/naturedatacube_app/app.R")

step 2. Select project Nestboxes and zoom in the map towards Arnhem and select the polygon
north of the A12 (under"De Hoge Veluwe")
step 3. select weather in available datasets 
step 4. select period and choose for this excersize from 2024-01-01 until 2025-01-01
step 5. select add to overview and Return data to R (close app)
data is now stored in the variable temp, ready for the next steps 


# ============================================================
# 4. Ensure lay_date is treated as a Date
# ============================================================
R sometimes reads dates as text.
We convert lay_date to a proper Date format.

data$lay_date <- as.Date(data$lay_date)


# ============================================================
# 5. Filter the nest data
# ============================================================
 We keep only:
 - data from 2024 onwards
 - nests with at least one egg
- nests from March to July (breeding season)
>>>>>>> 5bb21b5 (Add files via upload)

data <- data %>%
  filter(
    year >= 2024,
    clutch_size > 0,
    month(lay_date) %in% 3:7
  )

<<<<<<< HEAD
# 🐣 Mini challenge (optional): How many nests remain after filtering?
# Hint: use nrow(data)


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

# ============================================================
# 6. Extract weather data from the Shiny app output
# ============================================================
The Shiny app stores datasets inside `temp`.
Here we extract daily mean temperature data
and rename the columns for clarity.

weather <- temp$datasets$Weather_1 %>%
>>>>>>> 5bb21b5 (Add files via upload)
  select(
    date = datum,
    temp = mean_temperature
  )

<<<<<<< HEAD
# ============================================================
# 7. Make sure weather dates are Date objects
# ============================================================
mean_temp$date <- as.Date(mean_temp$date)
=======

# ============================================================
# 7. Ensure weather dates are also in Date format
# ============================================================

weather$date <- as.Date(weather$date)
>>>>>>> 5bb21b5 (Add files via upload)


# ============================================================
# 8. Merge nest data with temperature data
# ============================================================
<<<<<<< HEAD
# Each nest now gets the mean temperature on its lay date.

plot_data <- data %>%
  left_join(mean_temp, by = c("lay_date" = "date"))
=======
We join the two datasets so that each nest record
gets the temperature from the corresponding lay date.

plot_data <- data %>%
  left_join(weather, by = c("lay_date" = "date"))
>>>>>>> 5bb21b5 (Add files via upload)


# ============================================================
# 9. Keep only nests from 2024
# ============================================================
<<<<<<< HEAD
# This keeps the plot focused on a single breeding season.
=======
# This ensures the plot only shows data from one year.
>>>>>>> 5bb21b5 (Add files via upload)

plot_data <- plot_data %>%
  filter(year(lay_date) == 2024)


# ============================================================
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
# ============================================================
 This plot shows:
  - Lay date on the x-axis
 - Mean daily temperature on the y-axis
 - Point size represents clutch size
 - Point colour represents species

ggplot(plot_data, aes(x = lay_date, y = temp)) +
  geom_point(
    aes(size = clutch_size, colour = species),
    alpha = 0.4
>>>>>>> 5bb21b5 (Add files via upload)
  ) +
  scale_size_continuous(
    name = "Clutch size",
    range = c(1, 15)
  ) +
  labs(
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
    colour = "Species"
  ) +
  theme_minimal()
<<<<<<< HEAD
>>>>>>> 5bb21b5 (Add files via upload)
=======

>>>>>>> f4f57e4 (Revise tutorial for linking bird nest and weather data)
