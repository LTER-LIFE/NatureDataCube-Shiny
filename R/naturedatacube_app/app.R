<<<<<<< HEAD
# R shiny NatureDataCube interface demo app 
# Created on    : 30 October 2025
# Last update   : 19 December 2025

# Script to run the R Shiny interface for the NatureDataCube, demo version

# Load packages ----------------------------------------------------------------

library(here)
library(leaflet)
library(sf)
library(shiny)
library(tidyverse)
library(zip)

# API keys for KNMI data retrieval ---------------------------------------------

api_key <- "" # Put your Open data API key here, anonymous or user specific api key to access the KNMI data about the weather stations (request the Open data API key here: https://developer.dataplatform.knmi.nl/apis). Character string.
edr_api_key <- "" # Put your EDR API key here, user-specific KNMI EDR API key (request the EDR API key here: https://developer.dataplatform.knmi.nl/apis). Character string.

# Temporary "data" to test the app ---------------------------------------------

dataset_info <- read.csv(here::here("data/dataset_info_demo.csv"), header = TRUE, sep = ";")
dataset_info$start_date <- as.Date(dataset_info$start_date, format = "%d-%m-%Y")
dataset_info$end_date <- as.Date(dataset_info$end_date, format = "%d-%m-%Y")

# SNL beheertypen map (management areas) with only a subset of the polygons on the Veluwe (southwest)
beheer <- sf::read_sf(here::here('data/polygons/SNL_beheertypen_Veluwe_subset.shp'))
beheer_4326 <- sf::st_transform(beheer, 4326) %>%
  dplyr::mutate(row_id = as.character(dplyr::row_number()))

# Research sites on the Veluwe 
research <- sf::read_sf(here::here('data/polygons/merged_shapefiles_research_sites.shp'))
research_4326 <- sf::st_transform(research, 4326) %>%
  dplyr::mutate(row_id = as.character(dplyr::row_number()))

# Source retrieval functions
source(here::here("R/retrieval_functions/KNMI/get_daily_data_knmi.R"))
source(here::here("R/retrieval_functions/KNMI/get_daily_grid_data_knmi.R"))
source(here::here("R/retrieval_functions/KNMI/get_hourly_data_knmi.R"))

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("NatureDataCube"),
  
  # Extra UI choices -----------------------------------------------------------
  # Make sure that the full width is used for the text on the tabs of the chosen data set(s) (was especially a problem with the long variable names)
  tags$style(HTML("
  /* Checkboxes and radiobuttons use full width */
  .form-group.shiny-input-checkboxgroup,
  .form-group.shiny-input-radiogroup {
    width: 100% !important;
  }
  .shiny-options-group {
    display: block !important;
    width: 100% !important;
  }
  .checkbox label, .radio label {
    white-space: normal;
  }

  /* Text above choosing dates uses full width */
  #knmi_date_range label.control-label {
    display: block !important;
    width: 100% !important;
    white-space: nowrap !important;  
  }")),
  
  # Left panel------------------------------------------------------------------
  sidebarPanel(
    # 1. Select your area
    div(
      style = "margin-bottom: 10px;",
      radioButtons( 
        inputId = "chosen_area",
        label = "1. Select area:",
        choices = c("Upload shape file" = "user_area",
                    "Management sites" = "beheer_area",
                    "Research sites Veluwe" = "research_area"
        )
      ),
      conditionalPanel( # Upload shape file button appears when "upload my own shape file" is chosen
        condition = "input.chosen_area == 'user_area'",
        fileInput(
          "shapefile", 
          "Upload shape file of your area (ZIP or separate files):",
          multiple = TRUE,   
          accept = c('.zip', '.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'),
          buttonLabel = "Browse...",
          placeholder = "No file(s) selected"
        ),
        tags$style("
             .btn-file {  
               background-color: lightgrey; 
             }
             .progress-bar {
               background-color: green;
               border-color: black;
             }
        ")
      ),
      
      leafletOutput("map", height = "400px"),
      actionButton("zoom_poly", "Zoom to selected area", icon = icon("search"), style = "background-color: white; color: black;"), 
      br(), br(),
      
      # 2. Choose dataset(s)
      uiOutput("dataset_choices"), # Selection of datasets
      uiOutput("no_chosen_datasets_text"), # Text that tells user that available datasets will be there if shapefile has been uploaded or if polygon has been clicked
    ),
    br(),
    
    # 3. Select info per dataset
    p("3. Select info per dataset in the tabs on the right. 
      See what you have selected in the 'Overview table' above the tabs and click 'Download dataset(s)' to download all data.", 
      style = "font-weight:bold;"),
    br(),
    
  ),
  
  # Right panel-----------------------------------------------------------------
  mainPanel(
    fluidRow( # Overview (always visible)
      h4("Overview of selected datasets"),
      p("Check your selections in the table below and click 'Download dataset(s)'"),
      tableOutput("overview_table"), 
      div(
        style = "display: inline-flex; gap: 10px;",
        uiOutput("download_ui"),
        actionButton(
          "clear_table_messages", "Clear table and messages",
          icon = icon("trash")
        )
      ),
      br(),
    ),
    div(
      style = "margin-top: 10px;",
      uiOutput("download_messages")  
    ), 
    br(),
    
    # Only show dataset tabs when at least one dataset has been selected
    conditionalPanel(
      condition = "input.chosen_datasets && input.chosen_datasets.length > 0",
      fluidRow(
        uiOutput("dataset_tabs")
      )
=======
# ============================================================
# R Shiny NatureDataCube interface demo version: NAEM
# ============================================================
# Created on : 26 Jan 2026
# Last update : 30 Jan 2026
#
# Description:
# This script is based on the previous version created by Minke Mulder
# and this Shiny application allows users to:
#
# - Select a predefined study area or draw a custom polygon
# - Choose one or more NatureDataCube datasets
# - Configure dataset-specific parameters (for now only year)
# - Retrieve spatial data via the NatureDataCube API
# - Download results as GeoJSON files in a ZIP file or use them in a R variable
#
# how to run it:
# 1. first load / install the libraries
# 2. set the working directory to NatureDataCube-Shiny-NAEM
# 3. data_nc <- runApp("R/naturedatacube_app/app.R")
#
# Design rules for now:
# - ONE polygon (study area) per download / export
# - Multiple datasets are allowed for that polygon
# - Geometry is sent to the API as WKT (EPSG:4326)
# ============================================================


# ============================================================
# Libraries
# ============================================================

load_pkgs <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_install)) {
    install.packages(to_install)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

pkgs <- c(
  "shiny",
  "leaflet",
  "leaflet.extras",
  "sf",
  "dplyr",
  "purrr",
  "stringr",
  "httr",
  "geojsonsf",
  "jsonlite",
  "zip",
  "here",
  "terra",
  "lubridate"
)

load_pkgs(pkgs)


# ============================================================
# NatureDataCube API configuration
# ============================================================

# AgroDataCube
source(here::here("R", "retrieval_functions", "ndc_url.R"))
source(here::here("R", "retrieval_functions", "ndc_get.R"))

# GroenMonitor
source(here::here("R", "retrieval_functions", "gm_url.R"))
source(here::here("R", "retrieval_functions", "gm_get.R"))

# weather helper functions
source(here::here("R", "retrieval_functions", "weather_functions", "get_closest_meteostation.R"))
source(here::here("R", "retrieval_functions", "weather_functions", "get_meteo_for_date.R"))
source(here::here("R", "retrieval_functions", "weather_functions", "get_meteo_for_period.R"))
source(here::here("R", "retrieval_functions", "weather_functions", "get_meteo_for_long_period.R"))
source(here::here("R", "retrieval_functions", "weather_functions", "split_date_range.R"))

# NDVI
source(here::here("R","retrieval_functions", "ndvi", "monthly_ndvi.R"))
source(here::here("R","retrieval_functions", "ndvi", "monthly_ndvi_period.R"))

mytoken <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3N1ZWR0byI6ImQubGljaHRlbmJlcmdAbmlvby5rbmF3Lm5sIiwicmVzb3VyY2UiOlsiKiJdLCJpYXQiOjE3NjgyMTM3OTZ9.nEmOwkBTzKBjlsZL8obY-kWvghNS4A1M1Vwv1B94SSU"

myheaders <- c(
  "Accept" = "application/json;charset=utf-8",
  "token"  = mytoken
)


# ============================================================
# Load polygon data
# ============================================================
gpkg <- here::here("data", "study_sites.gpkg")
layers <- sf::st_layers(gpkg)$name

all_layers <- set_names(
  map(layers, ~ st_read(gpkg, layer = .x, quiet = TRUE)),
  layers
)

nutnet <- all_layers[["nutnet_poly"]]
nestboxes <- all_layers[str_detect(names(all_layers), "^nestkasten_")] %>%
  reduce(rbind)
loobos <- all_layers[["loobos"]]
lights <- all_layers[["20251008_licht_op_natuur_lantaarnpalen"]]


# ============================================================
# add WKT column
# ============================================================
add_wkt_column <- function(sf_obj, col_name = "wkt") {
  sf_obj <- st_transform(sf_obj, 4326)
  sf_obj[[col_name]] <- st_as_text(st_geometry(sf_obj))
  sf_obj
}

nutnet_wkt    <- add_wkt_column(nutnet)
nestboxes_wkt <- add_wkt_column(nestboxes)
loobos_wkt    <- add_wkt_column(loobos)
lights_wkt    <- add_wkt_column(lights)

# ============================================================
# drawn polygon to sf
# ============================================================

convert_drawn_to_sf <- function(feat, start_layer_id = 1) {
  if (is.null(feat) || feat$geometry$type != "Polygon") return(NULL)
  
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(
    rbind,
    lapply(coords, function(x) as.numeric(unlist(x)))
  )
  
  st_as_sf(st_sfc(st_polygon(list(coords)), crs = 4326)) %>%
    mutate(
      wkt = st_as_text(st_geometry(.)),
      layer_id = start_layer_id
    )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  # title
  titlePanel("Nature Data Cube"),
  
  # custom style
  tags$head(
    tags$style(HTML("
      .btn-custom {
        color: white !important;
        background-color: #28a745 !important;
        border: none !important;
        transition: all 0.2s ease-in-out;
      }
      .btn-custom:hover {
        background-color: #218838 !important;
        color: white !important;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "fixed_layer",
        "Select project or draw own polygon :",
        choices = c("NutNet", "Nestboxes", "Loobos", "Light on Nature")
      ),
      leafletOutput("map", height = "400px"),
      br(),
      h4("Choose dataset for the selected area"),
      radioButtons(
        "selected_dataset",
        "Available datasets:",
        choices = c("Agricultural fields", "AHN", "Soil map", "Weather", "NDVI")
      )
    ),
    
    mainPanel(
      h4("Overview of selected datasets"),
      uiOutput("overview_table"),
      actionButton("clear_overview", "Clear overview",
                   style = "color: white; background-color: grey;"),
      br(),
      uiOutput("download_ui"),
      br(),
      uiOutput("download_messages"),
      hr(),
      h4("Dataset metadata and selection"),
      uiOutput("dataset_metadata"),
      uiOutput("year_ui"),
      actionButton("add_dataset", "Add to overview")
>>>>>>> 5303c5d (Add files via upload)
    )
  )
)


<<<<<<< HEAD
# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  clicked_polygon <- reactiveVal(NULL)
  overview <- reactiveVal(
    data.frame(dataset = character(),
               sub_dataset = character(),
               stringsAsFactors = FALSE)
  )
  download_msgs <- reactiveVal(character(0))
  
  
  # Left panel -----------------------------------------------------------------
  #--------------------------------------------
  # Upload user shape file
  #--------------------------------------------
  user_shape <- reactive({
    req(input$chosen_area == "user_area")
    req(input$shapefile)
    temp_dir <- tempdir()
    input_user <- input$shapefile
    
    # ZIP
    zip_idx <- grepl("\\.zip$", input_user$name, ignore.case = TRUE)
    if (any(zip_idx)) {
      zip_file <- input_user$datapath[zip_idx][1]
      unzip_dir <- file.path(temp_dir, paste0("unzipped_", as.integer(Sys.time())))
      dir.create(unzip_dir, recursive = TRUE)
      unzip(zip_file, exdir = unzip_dir)
      shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
      if (length(shp_files) == 0) return(NULL)
      shp_path <- shp_files[1]
    } else { # Separate files
      files <- input$shapefile
      for (i in seq_len(nrow(files))) {
        file.copy(files$datapath[i],
                  file.path(temp_dir, files$name[i]),
                  overwrite = TRUE)
      }
      shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
      if (length(shp_files) == 0) return(NULL)
      shp_path <- shp_files[which.max(file.info(shp_files)$mtime)]
    }
    shp <- sf::st_read(shp_path, quiet = TRUE)
    sf::st_transform(shp, 4326)
  })
  
  #--------------------------------------------
  # Current area 
  #--------------------------------------------
  current_area <- reactive({
    if (input$chosen_area == "user_area") {
      user_shape()
    } else if (input$chosen_area %in% c("beheer_area", "research_area")) {
      clicked_polygon()
    } else {
      NULL
    }
  })
  
  #--------------------------------------------
  # Text when no upload and no selection of a polygon
  #--------------------------------------------
  output$no_chosen_datasets_text <- renderUI({
    if (input$chosen_area == "user_area" && is.null(input$shapefile)) {
      tags$p(
        "Available dataset(s) will be shown here when a shapefile has been uploaded.",
        style = "color: grey; font-style: italic;"
      )
    } else if (input$chosen_area %in% c("beheer_area", "research_area") && is.null(clicked_polygon())) {
      tags$p(
        "Available dataset(s) will be shown here when a polygon has been clicked on the map above.",
        style = "color: grey; font-style: italic;"
      )
    } else {
      NULL
    }
  })
  
  #--------------------------------------------
  # Map - first map you see 
  #--------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 5.3, lat = 52.1, zoom = 6) # Focus on the Netherlands
  })
  
  #--------------------------------------------
  # Map - zoom function
  #--------------------------------------------
  zoom_to_shape <- function(map_id, shp) {
    leafletProxy(map_id) %>%
      flyToBounds(
        lng1 = sf::st_bbox(shp)[["xmin"]],
        lat1 = sf::st_bbox(shp)[["ymin"]],
        lng2 = sf::st_bbox(shp)[["xmax"]],
        lat2 = sf::st_bbox(shp)[["ymax"]]
      )
  }
  
  #--------------------------------------------
  # Map - choosing SNL beheertypen or Research sites Veluwe
  #--------------------------------------------
  observeEvent(input$chosen_area, {
    leafletProxy("map") %>% 
      clearGroup("beheer_group") %>% 
      clearGroup("research_group") %>%
      clearGroup("user_group")
    
    if (input$chosen_area == "beheer_area") {
      leafletProxy("map") %>%
        addPolygons(
          data = beheer_4326,
          layerId = ~row_id,
          color = "black",
          fillColor = "black",
          fillOpacity = 0.3,
          weight = 3,
          group = "beheer_group"
        )
      zoom_to_shape("map", beheer_4326)
    } else if (input$chosen_area == "research_area") {
      leafletProxy("map") %>%
        addPolygons(
          data = research_4326,
          layerId = ~row_id,
          color = "black",
          fillColor = "black",
          fillOpacity = 0.3,
          weight = 3,
          group = "research_group"
        )
      zoom_to_shape("map", research_4326)
    } else { # Focus on the Netherlands when no shapefile has been uploaded yet
      shp <- user_shape()
      if (is.null(shp)) {
        leafletProxy("map") %>%
          setView(lng = 5.3, lat = 52.1, zoom = 6)
      } else {
        zoom_to_shape("map", shp)
      }
    }
  })
  
  #--------------------------------------------
  # Map - choosing upload my own shapefile
  #--------------------------------------------
  # This is in a separate observeEvent because otherwise the user_area is not shown on the map the first time when selected
  observeEvent(user_shape(), {
    req(input$chosen_area == "user_area")
    shp <- user_shape()
    req(shp)
    leafletProxy("map") %>% 
      clearGroup("user_group") %>% 
      clearGroup("beheer_group") %>%
      clearGroup("research_group") %>%
      addPolygons(
        data = shp,
        color = "#007bff",
        fillColor = "#007bff",
        fillOpacity = 0.3,
        weight = 3,
        group = "user_group"
      )
    zoom_to_shape("map", shp)
    
    # Update available datasets in step 2
    updateCheckboxGroupInput(
      session,
      "chosen_datasets",
      choices = unique(dataset_info$dataset)
    )
  })
  
  #--------------------------------------------
  # Map - zoom to selected area
  #--------------------------------------------
  observeEvent(input$zoom_poly, {
    if (input$chosen_area == "user_area") {
      zoom_to_shape("map", user_shape())
    } else if (input$chosen_area %in% c("beheer_area", "research_area")) {
      zoom_to_shape("map", clicked_polygon())
    }
  })
  
  #--------------------------------------------
  # Map - clicking on a polygon
  #--------------------------------------------
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)
    
    if (input$chosen_area == "beheer_area") {
      frag <- beheer_4326[beheer_4326$row_id == input$map_shape_click$id, ]
      
      # Convert multipolygon to single polygon
      poly <- frag |>
        sf::st_make_valid() |>
        sf::st_union() |>
        sf::st_cast("POLYGON") |>
        sf::st_as_sf()
      
      clicked_polygon(poly)
      
    } else if (input$chosen_area == "research_area") {
      poly <- research_4326[research_4326$row_id == input$map_shape_click$id, ]
    }
    clicked_polygon(poly)
    
    # Highlight polygon when clicked
    leafletProxy("map") %>%
      clearGroup("highlight_group") %>%   
      addPolygons(
        data = poly,
        color = "#007bff",        
        weight = 4,
        fillColor = "#007bff",
        fillOpacity = 0.5,
        group = "highlight_group"
      )
    
    # Update available datasets in step 2
    updateCheckboxGroupInput(
      session,
      "chosen_datasets",
      choices = unique(dataset_info$dataset)
    )
  })
  
  #--------------------------------------------
  # Dataset choices
  #--------------------------------------------
  output$dataset_choices <- renderUI({
    checkboxGroupInput(
      "chosen_datasets",
      "2. Choose available dataset(s) for the selected area:",
      choices = NULL
    )
  })

  #--------------------------------------------
  # New selected area - reset some things
  #--------------------------------------------
  observeEvent(input$chosen_area, {
    clicked_polygon(NULL)
    overview(data.frame(dataset = character(), sub_dataset = character()))
    download_msgs(character(0)) 
    leafletProxy("map") %>% clearGroup("highlight_group")
    output$dataset_choices <- renderUI({
      checkboxGroupInput(
        "chosen_datasets",
        "2. Choose available dataset(s) for the selected area:",
        choices = NULL
      )
    })
  })
  
  
  # Right panel ----------------------------------------------------------------
  
  #--------------------------------------------
  # Overview table - empty
  #--------------------------------------------
  empty_overview_row <- function(dataset, sub_dataset = NA,  variables = NA, start_date = NA, end_date = NA) {
    data.frame(
      dataset = dataset,
      sub_dataset = sub_dataset,
      variables = variable,
      start_date = start_date,
      end_date = end_date,
      stringsAsFactors = FALSE
    )
  }
  
  #--------------------------------------------
  # Overview table - no datasets
  #--------------------------------------------
  output$overview_table <- renderTable({
    if (nrow(overview()) == 0) {
      return(data.frame(Overview = "No datasets added yet."))
    } else {
      overview()
    }
  }, bordered = TRUE, striped = TRUE)
  
  #--------------------------------------------
  # Overview table - clear table and update messages
  #--------------------------------------------
  observeEvent(input$clear_table_messages, {
    overview(data.frame(dataset = character(), sub_dataset = character()))
    download_msgs(character(0)) 
  })
  
  #--------------------------------------------
  # Create dataset tabs 
  #--------------------------------------------
  output$dataset_tabs <- renderUI({
    req(input$chosen_datasets)
    tabs <- lapply(input$chosen_datasets, function(ds) {
      if (ds == "KNMI") {
        knmi_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == ds])
        tabPanel(title = ds, value = ds,
          br(),
          fluidRow(
            column(width = 6,
              tagList(
                h4(ds),
                radioButtons(inputId = "sub_knmi", label = "Select subdataset:", choices = knmi_subs),
                uiOutput("knmi_options"),
                actionButton(inputId = "add_knmi", label = "Add to overview table"))),
            column(width = 6,
              wellPanel(
                h4(strong("Dataset information")),
                uiOutput("knmi_dataset_desc"),
                br(),
                uiOutput("knmi_subdataset_desc")))))
        } else if (ds == "Soil") {
          soil_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == ds])
          tabPanel(title = ds, value = ds,
                   br(),
                   fluidRow(
                     column(width = 6,
                            tagList(
                              h4(ds),
                              radioButtons(inputId = "sub_soil", label = "Select subdataset:", choices = soil_subs),
                              uiOutput("soil_options"),
                              actionButton(inputId = "add_soil", label = "Add to overview table"))),
                     column(width = 6,
                            wellPanel(
                              h4(strong("Dataset information")),
                              uiOutput("soil_dataset_desc"),
                              br(),
                              uiOutput("soil_subdataset_desc")))))
          } else if (ds == "NDVI") {
          ndvi_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == ds])
          tabPanel(title = ds, value = ds,
                   br(),
                   fluidRow(
                     column(width = 6,
                            tagList(
                              h4(ds),
                              radioButtons(inputId = "sub_ndvi", label = "Select subdataset:", choices = ndvi_subs),
                              uiOutput("ndvi_options"),
                              actionButton(inputId = "add_ndvi", label = "Add to overview table"))),
                     column(width = 6,
                            wellPanel(
                              h4(strong("Dataset information")),
                              uiOutput("ndvi_dataset_desc"),
                              br(),
                              uiOutput("ndvi_subdataset_desc")))))
          } else if (ds == "AHN") {
            ahn_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == ds])
            tabPanel(title = ds, value = ds,
                     br(),
                     fluidRow(
                       column(width = 6,
                              tagList(
                                h4(ds),
                                radioButtons(inputId = "sub_ahn", label = "Select subdataset:", choices = ahn_subs),
                                uiOutput("ahn_options"),
                                actionButton(inputId = "add_ahn", label = "Add to overview table"))),
                       column(width = 6,
                              wellPanel(
                                h4(strong("Dataset information")),
                                uiOutput("ahn_dataset_desc"),
                                br(),
                                uiOutput("ahn_subdataset_desc")))))
          } else if (ds == "LiDAR vegetation") {
            lidar_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == ds])
            tabPanel(title = ds, value = ds,
                     br(),
                     fluidRow(
                       column(width = 6,
                              tagList(
                                h4(ds),
                                radioButtons(inputId = "sub_lidar", label = "Select subdataset:", choices = lidar_subs),
                                uiOutput("lidar_options"),
                                actionButton(inputId = "add_lidar", label = "Add to overview table"))),
                       column(width = 6,
                              wellPanel(
                                h4(strong("Dataset information")),
                                uiOutput("lidar_dataset_desc"),
                                br(),
                                uiOutput("lidar_subdataset_desc")))))
          } else if (ds == "Satellite") {
            sat_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == ds])
            tabPanel(title = ds, value = ds,
                     br(),
                     fluidRow(
                       column(width = 6,
                              tagList(
                                h4(ds),
                                radioButtons(inputId = "sub_sat", label = "Select subdataset:", choices = sat_subs),
                                uiOutput("sat_options"),
                                actionButton(inputId = "add_sat", label = "Add to overview table"))),
                       column(width = 6,
                              wellPanel(
                                h4(strong("Dataset information")),
                                uiOutput("sat_dataset_desc"),
                                br(),
                                uiOutput("sat_subdataset_desc")))))
          } else {
            tabPanel(title = ds,value = ds,
                     tagList(
                       h4(ds),
                       p("Nothing here yet")
                       )
                     )
            }
      })
    do.call(tabsetPanel, c(id = "tabs", tabs))
  })
  
  #--------------------------------------------
  # Dataset tab: dataset description
  #-------------------------------------------- 
  dataset_desc <- function(ds) {
    renderUI({
      selected_info <- subset(dataset_info, dataset == ds)
      desc_dataset <- paste(unique(selected_info$description), collapse = "\n")
      
      # Make URLs clickable
      desc_html <- gsub(
        "(https?://\\S+)", 
        '<a href="\\1" target="_blank">\\1</a>', 
        desc_dataset
      )
      HTML(paste0('<strong>Description of dataset</strong><br>', desc_html))
    })
  }
  
  # Description for each dataset
  output$knmi_dataset_desc    <- dataset_desc("KNMI")
  output$ndvi_dataset_desc    <- dataset_desc("NDVI")
  output$ahn_dataset_desc     <- dataset_desc("AHN")
  output$lidar_dataset_desc   <- dataset_desc("LiDAR vegetation")
  output$sat_dataset_desc     <- dataset_desc("Satellite")
  output$soil_dataset_desc    <- dataset_desc("Soil")
  
  #--------------------------------------------
  # Dataset tab: sub dataset description
  #-------------------------------------------- 
  subdataset_desc <- function(dataset_name, input_id) {
    renderUI({
      selected_info <- subset(dataset_info, dataset == dataset_name & sub_dataset == input[[input_id]])
      desc_sub <- paste(unique(selected_info$description_sub), collapse = "\n")
      
      # Make URLs clickable
      desc_sub_html <- gsub(
        "(https?://\\S+)", 
        '<a href="\\1" target="_blank">\\1</a>', 
        desc_sub
      )
      HTML(paste0('<strong>Description of subdataset</strong><br>', desc_sub_html))
    })
  }
  
  # Description for each subdataset
  output$knmi_subdataset_desc   <- subdataset_desc("KNMI", "sub_knmi")
  output$ndvi_subdataset_desc   <- subdataset_desc("NDVI", "sub_ndvi")
  output$ahn_subdataset_desc    <- subdataset_desc("AHN", "sub_ahn")
  output$lidar_subdataset_desc  <- subdataset_desc("LiDAR vegetation", "sub_lidar")
  output$sat_subdataset_desc    <- subdataset_desc("Satellite", "sub_sat")
  output$soil_subdataset_desc   <- subdataset_desc("Soil", "sub_soil")
  
  #--------------------------------------------
  # Dataset tab: KNMI climate - options
  #-------------------------------------------- 
  output$knmi_options <- renderUI({
    req(input$sub_knmi)
    sub_ds <- input$sub_knmi
    info <- dataset_info %>%
      dplyr::filter(dataset == "KNMI", sub_dataset == sub_ds)
    tagList(
      dateRangeInput(
        inputId = "knmi_date_range",
        label = "From and until which date do you want data?",
        start = min(info$start_date),
        end = max(info$end_date),
        min = min(info$start_date),
        max = max(info$end_date),
        format = "dd-mm-yyyy",
        language = "eng",
        weekstart = 1
      ),
      checkboxGroupInput(
        inputId = "knmi_variable",
        label = "Select variables:",
        choices = unique(info$variable)
      )
    )
  })
  
  #--------------------------------------------
  # Dataset tab: NDVI - options
  #-------------------------------------------- 
  output$ndvi_options <- renderUI({
    req(input$sub_ndvi)
    sub_ds <- input$sub_ndvi
    info <- dataset_info %>%
      dplyr::filter(dataset == "NDVI", sub_dataset == sub_ds)
    if (sub_ds == "Map") {
      tagList(
        dateRangeInput(
          inputId = "ndvi_date_range",
          label = "From and until which date do you want data?",
          start = min(info$start_date),
          end = max(info$end_date),
          min = min(info$start_date),
          max = max(info$end_date),
          format = "dd-mm-yyyy",
          language = "eng",
          weekstart = 1
        )
      )
    } else if (sub_ds == "Zonal statistics") {
      tagList(
        dateInput(
          inputId = "ndvi_date",
          label = "For which date do you want data?",
          value = min(info$start_date),
          min = min(info$start_date),
          max = max(info$end_date),
          format = "dd-mm-yyyy",
          language = "eng",
          weekstart = 1
        )
      )
    } else {
      NULL
    }
  })
  
  #--------------------------------------------
  # Dataset tab: Soil - options
  #-------------------------------------------- 
  output$soil_options <- renderUI({
    req(input$sub_soil)
    sub_ds <- input$sub_soil
    info <- dataset_info %>%
      dplyr::filter(dataset == "Soil", sub_dataset == sub_ds)
    tagList(
      dateInput(
        inputId = "soil_date",
        label = "For which date do you want data?",
        value = min(info$start_date),
        min = min(info$start_date),
        max = max(info$end_date),
        format = "dd-mm-yyyy",
        language = "eng",
        weekstart = 1
      )
    )
  })
  
  #--------------------------------------------
  # Dataset tab: AHN - options
  #-------------------------------------------- 
  output$ahn_options <- renderUI({
    req(input$sub_ahn)
    sub_ds <- input$sub_ahn
    info <- dataset_info %>%
      dplyr::filter(dataset == "AHN", sub_dataset == sub_ds)
    tagList(
      dateInput(
        inputId = "ahn_date",
        label = "For which date do you want data?",
        value = min(info$start_date),
        min = min(info$start_date),
        max = max(info$end_date),
        format = "dd-mm-yyyy",
        language = "eng",
        weekstart = 1
      )
    )
  })
  
  #--------------------------------------------
  # Dataset tab: Satellite - options
  #-------------------------------------------- 
  output$sat_options <- renderUI({
    req(input$sub_sat)
    sub_ds <- input$sub_sat
    info <- dataset_info %>%
      dplyr::filter(dataset == "Satellite", sub_dataset == sub_ds)
    tagList(
      dateInput(
        inputId = "sat_date",
        label = "For which date do you want data?",
        value = min(info$start_date),
        min = min(info$start_date),
        max = max(info$end_date),
        format = "dd-mm-yyyy",
        language = "eng",
        weekstart = 1
      )
    )
  })
  
  #--------------------------------------------
  # Dataset tab: LiDAR vegetation - options
  #-------------------------------------------- 
  output$lidar_options <- renderUI({
    req(input$sub_lidar)
    sub_ds <- input$sub_lidar
    info <- dataset_info %>%
      dplyr::filter(dataset == "LiDAR vegetation", sub_dataset == sub_ds)
    tagList(
      dateInput(
        inputId = "lidar_date",
        label = "For which date do you want data?",
        value = min(info$start_date),
        min = min(info$start_date),
        max = max(info$end_date),
        format = "dd-mm-yyyy",
        language = "eng",
        weekstart = 1
      )
    )
  })
  
  #--------------------------------------------
  # Overview table - add KNMI info 
  #-------------------------------------------- 
  observeEvent(input$add_knmi, {
    current <- overview()
    sub_val <- input$sub_knmi
    var_val <- input$knmi_variable
    date_vals <- input$knmi_date_range
    if (is.null(sub_val) || is.null(date_vals)  || is.null(var_val) || length(var_val) == 0 || length(date_vals) != 2) {
      showNotification(
        "Please fill in everything (subdataset, date range and variables) before adding to overview table.",
        type = "error",
        duration = 4
      )
      return()
    }
    var_val_str <- paste(var_val, collapse = ", ")
    start_val_display <- format(as.Date(date_vals[1]), "%d-%m-%Y")
    end_val_display   <- format(as.Date(date_vals[2]), "%d-%m-%Y")
    exists <- any(
      current$dataset == "KNMI" &
        current$sub_dataset == sub_val &
        current$variables == var_val_str &
        current$start_date == start_val_display &
        current$end_date == end_val_display 
    )
    if (exists) {
      showNotification(
        "This KNMI selection is already in the table, so it will not be added a second time.",
        type = "warning",
        duration = 3
      )
      return()  
    }
    new_row <- data.frame(
      dataset    = "KNMI",
      sub_dataset= sub_val,
      variables  = var_val_str,
      start_date = start_val_display,   
      end_date   = end_val_display,  
      stringsAsFactors = FALSE
    )
    overview(bind_rows(current, new_row))
  })
  
  #--------------------------------------------
  # Overview table - add Soil info 
  #-------------------------------------------- 
  observeEvent(input$add_soil, {
    current <- overview()
    sub_val <- input$sub_soil
    start_val <- input$soil_date
    end_val   <- input$soil_date   
    
    if (is.null(sub_val)) {
      showNotification(
        "Please fill in the subdataset before adding to overview table.",
        type = "error",
        duration = 4
      )
      return()
    }
    start_val_display <- format(as.Date(start_val), "%d-%m-%Y")
    end_val_display   <- format(as.Date(end_val),   "%d-%m-%Y")
    exists <- any(
      current$dataset == "Soil" &
        current$sub_dataset == sub_val &
        current$start_date == start_val_display &
        current$end_date == end_val_display
    )
    if (exists) {
      showNotification(
        "This soil selection is already in the table, so it will not be added a second time.",
        type = "warning",
        duration = 3
      )
      return()  
    }
    new_row <- data.frame(
      dataset    = "Soil",
      sub_dataset= sub_val,
      start_date  = start_val_display,
      end_date    = end_val_display,
      stringsAsFactors = FALSE
    )
    overview(bind_rows(current, new_row))
  })
  
  #--------------------------------------------
  # Overview table - add NDVI info 
  #-------------------------------------------- 
  observeEvent(input$add_ndvi, {
    current <- overview()
    sub_val <- input$sub_ndvi
    
    if (sub_val == "Map") {
      req(input$ndvi_date_range)
      start_val <- input$ndvi_date_range[1]
      end_val   <- input$ndvi_date_range[2]
      
    } else if (sub_val == "Zonal statistics") {
      req(input$ndvi_date)
      start_val <- input$ndvi_date
      end_val   <- input$ndvi_date   
      
    } else {
      showNotification(
        "Please choose valid NDVI settings.",
        type = "error",
        duration = 4
      )
      return()
    }
    start_val_display <- format(as.Date(start_val), "%d-%m-%Y")
    end_val_display   <- format(as.Date(end_val),   "%d-%m-%Y")
    exists <- any(
      current$dataset == "NDVI" &
        current$sub_dataset == sub_val &
        current$start_date == start_val_display &
        current$end_date == end_val_display
    )
    if (exists) {
      showNotification(
        "This NDVI selection already exists in the table.",
        type = "warning",
        duration = 3
      )
      return()
    }
    new_row <- data.frame(
      dataset     = "NDVI",
      sub_dataset = sub_val,
      start_date  = start_val_display,
      end_date    = end_val_display,
      stringsAsFactors = FALSE
    )
    overview(bind_rows(current, new_row))
  })
  
  #--------------------------------------------
  # Overview table - add AHN info 
  #-------------------------------------------- 
  observeEvent(input$add_ahn, {
    current <- overview()
    sub_val <- input$sub_ahn
    start_val <- input$ahn_date
    end_val   <- input$ahn_date   
    if (is.null(sub_val)) {
      showNotification(
        "Please fill in the subdataset before adding to overview table.",
        type = "error",
        duration = 4
      )
      return()
    }
    start_val_display <- format(as.Date(start_val), "%d-%m-%Y")
    end_val_display   <- format(as.Date(end_val),   "%d-%m-%Y")
    exists <- any(
      current$dataset == "AHN" &
        current$sub_dataset == sub_val &
        current$start_date == start_val_display &
        current$end_date == end_val_display
    )
    if (exists) {
      showNotification(
        "This AHN selection is already in the table, so it will not be added a second time.",
        type = "warning",
        duration = 3
      )
      return()  
    }
    new_row <- data.frame(
      dataset    = "AHN",
      sub_dataset= sub_val,
      start_date = start_val_display,   
      end_date   = end_val_display,  
      stringsAsFactors = FALSE
    )
    overview(bind_rows(current, new_row))
  })
  
  #--------------------------------------------
  # Overview table - add LiDAR vegetation info 
  #-------------------------------------------- 
  observeEvent(input$add_lidar, {
    current <- overview()
    sub_val <- input$sub_lidar
    start_val <- input$lidar_date
    end_val   <- input$lidar_date  
    if (is.null(sub_val)) {
      showNotification(
        "Please fill in the subdataset before adding to overview table.",
        type = "error",
        duration = 4
      )
      return()
    }
    start_val_display <- format(as.Date(start_val), "%d-%m-%Y")
    end_val_display   <- format(as.Date(end_val),   "%d-%m-%Y")
    exists <- any(
      current$dataset == "LiDAR vegetation" &
        current$sub_dataset == sub_val &
        current$start_date == start_val_display &
        current$end_date == end_val_display 
    )
    if (exists) {
      showNotification(
        "This LiDAR vegetation selection is already in the table, so it will not be added a second time.",
        type = "warning",
        duration = 3
      )
      return()  
    }
    new_row <- data.frame(
      dataset    = "LiDAR vegetation",
      sub_dataset= sub_val,
      start_date = start_val_display,   
      end_date   = end_val_display,  
      stringsAsFactors = FALSE
    )
    overview(bind_rows(current, new_row))
  })
  
  #--------------------------------------------
  # Overview table - add Satellite info 
  #-------------------------------------------- 
  observeEvent(input$add_sat, {
    current <- overview()
    sub_val <- input$sub_sat
    start_val <- input$sat_date
    end_val   <- input$sat_date  
    
    if (is.null(sub_val)) {
      showNotification(
        "Please fill in the subdataset before adding to overview table.",
        type = "error",
        duration = 4
      )
      return()
    }
    start_val_display <- format(as.Date(start_val), "%d-%m-%Y")
    end_val_display   <- format(as.Date(end_val),   "%d-%m-%Y")
    exists <- any(
      current$dataset == "Satellite" &
        current$sub_dataset == sub_val &
        current$start_date == start_val_display &
        current$end_date == end_val_display
    )
    if (exists) {
      showNotification(
        "This LiDAR vegetation selection is already in the table, so it will not be added a second time.",
        type = "warning",
        duration = 3
      )
      return()  
    }
    new_row <- data.frame(
      dataset     = "Satellite",
      sub_dataset = sub_val,
      start_date  = start_val_display,
      end_date    = end_val_display,
      stringsAsFactors = FALSE
    )
    overview(bind_rows(current, new_row))
  })
  
  #--------------------------------------------
  # Appearing download button
  #--------------------------------------------
  output$download_ui <- renderUI({
    if (nrow(overview()) == 0) {
      # No datasets in overview table
      return(
        div(
          style = "color: grey; font-style: italic; padding: 6px 12px; border: 1px solid #ccc; border-radius: 4px;",
          "The download button will appear here when you add at least one dataset to the overview table."
        )
      )
    } else {
      # At least one dataset in overview table
      downloadButton(
        "download_data", "4. Download dataset(s)",
        icon = icon("download"),
        style = "background-color: #007bff; color: white;"
      )
    }
  })
  
  #--------------------------------------------
  # Download messages
  #--------------------------------------------
  output$download_messages <- renderUI({
    msgs <- download_msgs()
    if (length(msgs) == 0) {
      p("Updates about retrieving datasets will be here.",
        style = "color: grey; font-style: italic;")
    } else {
      HTML(paste0(msgs, collapse = "<br>"))
    }
  })
  
  #--------------------------------------------
  # Retrieve and download
  #--------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() paste0("naturedatacube_data_", Sys.Date(), ".zip"),
    content = function(zipfile) {
      
      ov <- overview()
      if (nrow(ov) == 0) {
        showNotification("No datasets in the overview table.", type = "error")
        return(NULL)
      }
      
      tmpdir <- tempdir()
      workdir <- file.path(tmpdir, "export")
      if (dir.exists(workdir)) unlink(workdir, recursive = TRUE)
      dir.create(workdir)
      
      download_msgs(character(0))  
      
      n <- nrow(ov)
      
      withProgress(message = "Retrieving datasets...", value = 0, {
        
        for (i in seq_len(n)) {
          
          ds        <- ov$dataset[i]
          sub       <- ov$sub_dataset[i]
          var       <- ov$variables[i]
          start     <- as.Date(ov$start_date[i], format = "%d-%m-%Y")
          end       <- as.Date(ov$end_date[i],   format = "%d-%m-%Y")
          safe_sub  <- gsub("[^A-Za-z0-9_]+", "_", sub)
          
          outfile <- NULL
          row_messages <- character(0)
          success <- FALSE
          
          # KNMI requires weather vars cleaned for retrieval
          if (ds == "KNMI") {
            weather_vars <- unlist(strsplit(var, ","))
            weather_vars <- trimws(sapply(weather_vars, function(x) strsplit(x, ":")[[1]][1]))
            
          }

          tryCatch({
            
            withCallingHandlers({
              
              if (ds == "KNMI") {
                if (sub == "Daily grid") {
                  knmi_data <- get_daily_grid_data_knmi(
                    area = current_area(),
                    start_date = format(start, "%Y%m%d"),
                    end_date   = format(end, "%Y%m%d"),
                    weather_var = weather_vars,
                    api_key = edr_api_key
                  )
                  
                  for (variable in names(knmi_data)) {
                    outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_", variable, "_row_", i, ".nc"))
                    terra::writeCDF(knmi_data[[variable]], outfile, overwrite = TRUE)
                  }
                  
                } else if (sub == "Daily in situ") {
                  knmi_data <- get_daily_data_knmi(
                    area = current_area(),
                    start_date = format(start, "%Y%m%d"),
                    end_date   = format(end, "%Y%m%d"),
                    weather_var = weather_vars,
                    api_key = api_key
                  )
                  outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_row_", i, ".csv"))
                  write.csv(knmi_data, outfile, row.names = FALSE)
                } else if (sub == "Hourly in situ") {
                  knmi_data <- get_hourly_data_knmi(
                    area = current_area(),
                    start_date = format(start, "%Y%m%d"),
                    end_date   = format(end, "%Y%m%d"),
                    weather_var = weather_vars,
                    api_key = api_key
                  )
                  outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_row_", i, ".csv"))
                  write.csv(knmi_data, outfile, row.names = FALSE)
                }
              }
              },

            message = function(m) {
              row_messages <<- c(row_messages, conditionMessage(m))
            })
            
            success <- TRUE
            
          }, error = function(e) {
            row_messages <<- c(row_messages, paste0("ERROR: ", e$message))
            success <- FALSE
          })
          
          if (success && !is.null(outfile) && file.exists(outfile)) {
            download_msgs(c(download_msgs(),
                            paste0("✔ <b>Retrieved:</b> ", ds, " - ", sub, " (row ", i, ")")
            ))
          } else {
            download_msgs(c(download_msgs(),
                            paste0("❌ <b>Failed to retrieve:</b> ", ds, " - ", sub,
                                   " (row ", i, "). No output file was created.")
            ))
          }

          if (length(row_messages) > 0) {
            
            extra_note <- ""
            
            # Detect KNMI API errors
            if (any(grepl("403|Forbidden", row_messages, ignore.case = TRUE))) {
              extra_note <- " Check your API key, it might be missing or invalid."
            }
            
            download_msgs(c(
              download_msgs(),
              paste0(
                paste(row_messages, collapse = "<br>"),
                extra_note
              )
            ))
          }
          
          incProgress(1/n, detail = paste(ds, "-", sub, "row", i))
        }  
      })  
      
      download_msgs(c(download_msgs(), "<b>Retrieval process is done.</b>"))
      
      files_created <- list.files(workdir)

      oldwd <- setwd(workdir)
      on.exit(setwd(oldwd))
      zip(zipfile, files = files_created)
      
      

    }
  )
  

  
 
}




# ------------------------------------------------------------------------------

shinyApp(ui, server)


=======
# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  
  drawn_polygon <- reactiveVal(NULL)
  fixed_polys   <- reactiveVal(NULL)
  
  overview <- reactiveVal(
    tibble::tibble(
      dataset = character(),
      year = integer(),
      polygon = character(),
      wkt = character(),
      polygon_sf = list(),
      date_from = as.Date(character()),
      date_to = as.Date(character())
    )
  )
  
  download_msgs <- reactiveVal(character(0))
  
  # --- Map ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 5.3, lat = 52.1, zoom = 7) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polygonOptions = drawPolygonOptions(showArea = TRUE, repeatMode = FALSE),
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, remove = TRUE)
      )
  })
  
  # Fixed polygon selection
  observeEvent(input$fixed_layer, {
    leafletProxy("map") %>%
      clearGroup("fixed") %>%
      clearGroup("highlight_fixed") %>%
      clearPopups()
    
    poly <- switch(
      input$fixed_layer,
      "NutNet" = nutnet_wkt,
      "Nestboxes" = nestboxes_wkt,
      "Loobos" = loobos_wkt,
      "Light on Nature" = lights_wkt
    ) %>%
      mutate(layer_id = row_number())
    
    fixed_polys(poly)
    
    leafletProxy("map") %>% addPolygons(
      data = poly,
      group = "fixed",
      color = "black",
      fillOpacity = 0.3,
      weight = 2,
      layerId = ~layer_id
    )
    
    if (is.null(drawn_polygon())) {
      centroid <- sf::st_centroid(poly[1, ])
      coords <- sf::st_coordinates(centroid)
      
      leafletProxy("map") %>% addPopups(
        lng = coords[1],
        lat = coords[2],
        popup = "⚠ You have selected a project but still need to select or draw a project area.",
        options = popupOptions(closeButton = TRUE)
      )
    }
  })
  
  # Clicking a fixed polygon
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    
    poly <- fixed_polys() %>% filter(layer_id == click$id)
    
    if (nrow(poly) > 0) {
      drawn_polygon(poly)
      leafletProxy("map") %>%
        clearGroup("highlight_fixed") %>%
        addPolygons(
          data = poly,
          group = "highlight_fixed",
          color = "#007bff",
          weight = 4,
          fillOpacity = 0.5
        )
    }
  })
  
  # Drawing a custom polygon
  observeEvent(input$map_draw_new_feature, {
    start_id <- if (is.null(fixed_polys())) 1 else max(fixed_polys()$layer_id, na.rm = TRUE) + 1
    poly_sf <- convert_drawn_to_sf(input$map_draw_new_feature, start_layer_id = start_id)
    req(poly_sf)
    
    drawn_polygon(poly_sf)
    
    leafletProxy("map") %>%
      clearGroup("highlight_drawn") %>%
      clearGroup("highlight_fixed") %>%
      addPolygons(
        data = poly_sf,
        group = "highlight_drawn",
        color = "#007bff",
        weight = 4,
        fillOpacity = 0.5,
        layerId = ~layer_id
      )
  })
  
  observeEvent(input$map_draw_deleted_features, {
    drawn_polygon(NULL)
    leafletProxy("map") %>% clearGroup("highlight_drawn")
  })
  
  # Metadata UI
  output$dataset_metadata <- renderUI({
    req(input$selected_dataset)
    switch(
      input$selected_dataset,
      "Agricultural fields" = HTML("Agricultural fields: Data about crops, yield, and fertilization."),
      "AHN" = HTML("AHN: Elevation data of the Netherlands. Provided as summary statistics only: minimum, maximum and mean (only AHN4 is currently available)."),
      "Soil map" = HTML("Soil map: Soil map for the selected area."),
      "Weather" = HTML("weather data retrieved from KNMI for selected area"),
      "NDVI" = HTML("Monthly average NDVI for selected area")
    )
  })
  
  # Dataset-specific options
  output$year_ui <- renderUI({
    req(input$selected_dataset)
    
    if (input$selected_dataset == "Agricultural fields") {
      numericInput("selected_year", "Select year:", value = 2025, min = 2020, max = 2025)
      
    } else if (input$selected_dataset == "Weather") {
      tagList(
        radioButtons(
          "weather_mode",
          "Weather query type:",
          choices = c("Single date" = "single", "Period" = "period"),
          selected = "single",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.weather_mode == 'single'",
          dateInput(
            "weather_date",
            "Date (single):",
            value = as.Date("2025-01-01"),
            min = as.Date("2017-01-01"),
            max = as.Date("2025-01-01")
          )
        ),
        conditionalPanel(
          condition = "input.weather_mode == 'period'",
          dateRangeInput(
            "weather_period",
            "From - To:",
            start = as.Date("2024-12-01"),
            end = as.Date("2025-01-01"),
            min = as.Date("2017-01-01"),
            max = as.Date("2025-01-01")
          )
        )
      )
      
    } else if (input$selected_dataset == "NDVI") {
      tagList(
        radioButtons(
          "ndvi_mode", 
          "NDVI query type:",
          choices = c("Single month" = "single", "Range of months" = "range"),
          selected = "single",
          inline = TRUE
        ),
        
        # Single month
        conditionalPanel(
          condition = "input.ndvi_mode == 'single'",
          numericInput("ndvi_year",  "Year:", value = 2025, min = 2017, max = 2025),
          numericInput("ndvi_month", "Month (1-12):", value = 1,  min = 1, max = 12)
        ),
        
        # Range of months
        conditionalPanel(
          condition = "input.ndvi_mode == 'range'",
          fluidRow(
            column(6,
                   numericInput("ndvi_from_year", "From Year:", value = 2025, min = 2017, max = 2025),
                   numericInput("ndvi_from_month", "From Month (1-12):", value = 1, min = 1, max = 12)
            ),
            column(6,
                   numericInput("ndvi_to_year", "To Year:", value = 2025, min = 2017, max = 2025),
                   numericInput("ndvi_to_month", "To Month (1-12):", value = 12, min = 1, max = 12)
            )
          )
        )
      )
      
    } else {
      helpText("This dataset does not require a year or date selection.")
    }
  })
  
  
  
  # Add to overview
  observeEvent(input$add_dataset, {
    poly <- drawn_polygon()
    
    if (is.null(poly)) {
      showNotification("⚠ Please select or draw a polygon first.", type = "error", duration = 5)
      return(NULL)
    }
    
    req(input$selected_dataset)
    
    # default values
    year_val <- NA_integer_
    date_from_val <- as.Date(NA)
    date_to_val <- as.Date(NA)
    
    # dataset-specific handling
    if (input$selected_dataset == "Agricultural fields") {
      # agricultural fields needs a year
      req(input$selected_year)
      year_val <- as.integer(input$selected_year)
      
    } else if (input$selected_dataset == "NDVI") {
      # NDVI: can be single month or range
      if (input$ndvi_mode == "single") {
        # single month -> mark that month as a single-day range (or first-to-last day)
        year_val <- NA_integer_
        date_from_val <- as.Date(sprintf("%04d-%02d-01", input$ndvi_year, input$ndvi_month))
        # last day of that month
        date_to_val <- (as.Date(sprintf("%04d-%02d-01", input$ndvi_year, input$ndvi_month)) + months(1)) - 1
      } else {
        # range of months
        year_val <- NA_integer_
        date_from_val <- as.Date(sprintf("%04d-%02d-01", input$ndvi_from_year, input$ndvi_from_month))
        next_month <- as.Date(sprintf("%04d-%02d-01", input$ndvi_to_year, input$ndvi_to_month)) + months(1)
        date_to_val <- next_month - 1
      }
      
    } else if (input$selected_dataset == "Weather") {
      # Weather: single date or period
      if (is.null(input$weather_mode) || input$weather_mode == "single") {
        date_from_val <- as.Date(input$weather_date)
        date_to_val   <- as.Date(input$weather_date)
      } else {
        date_from_val <- as.Date(input$weather_period[1])
        date_to_val   <- as.Date(input$weather_period[2])
      }
      year_val <- NA_integer_
      
    } else if (input$selected_dataset %in% c("AHN", "Soil map")) {
      # these don't need year/date
      year_val <- NA_integer_
    } else {
      # fallback: if your UI adds other datasets in future
      year_val <- NA_integer_
    }
    
    # determine polygon name (fixed project name or "Own polygon")
    poly_name <- if (!is.null(poly$layer_id) && !is.null(fixed_polys())) {
      fixed_poly <- fixed_polys() %>% filter(layer_id == poly$layer_id)
      if (nrow(fixed_poly) > 0) input$fixed_layer else "Own polygon"
    } else {
      "Own polygon"
    }
    
    # Only allow one study area in the overview
    ov <- overview()
    if (nrow(ov) > 0) {
      if (!identical(ov$wkt[1], poly$wkt[1])) {
        showModal(modalDialog(
          title = "Only one study area allowed",
          paste0(
            "The overview already contains datasets for a different polygon.\n\n",
            "You can add multiple datasets but only for a single polygon at a time.\n\n",
            "To add datasets for this new polygon, please first clear the overview."
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
    }
    
    # duplicate check: treat NA comparisons carefully
    duplicate_check <- ov %>%
      filter(
        dataset == input$selected_dataset,
        (is.na(year) & is.na(year_val) | year == year_val),
        polygon == poly_name,
        (is.na(date_from) & is.na(date_from_val) | date_from == date_from_val),
        (is.na(date_to) & is.na(date_to_val) | date_to == date_to_val)
      )
    
    if (nrow(duplicate_check) == 0) {
      overview(bind_rows(
        ov,
        tibble::tibble(
          dataset = input$selected_dataset,
          year = year_val,
          polygon = poly_name,
          wkt = poly$wkt[1],
          polygon_sf = list(poly),
          date_from = date_from_val,
          date_to = date_to_val
        )
      ))
    } else {
      showNotification("This dataset + polygon + year/date is already in the overview.", type = "message")
    }
  })
  
  
  # Overview table
  output$overview_table <- renderUI({
    ov <- overview()
    if (nrow(ov) == 0)
      return(p("No datasets added yet.", style = "color: grey; font-style: italic;"))
    
    tags$table(
      class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Dataset"),
          tags$th("Year"),
          tags$th("Polygon"),
          tags$th("Delete")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(ov)), function(i) {
          row <- ov[i, ]
          tags$tr(
            tags$td(row$dataset),
            tags$td(ifelse(is.na(row$year), "", row$year)),
            tags$td(row$polygon),
            tags$td(
              # use Shiny.setInputValue to send a single delete event with the row index
              actionButton(
                paste0("delete_row_", i),
                "x",
                onclick = sprintf(
                  "Shiny.setInputValue('delete_row', %d, {priority: 'event'})",
                  i
                ),
                style = "color:white;background-color:#CD5C5C;padding:2px 5px;font-size:80%;"
              )
            )
          )
        })
      )
    )
  })
  
  
  # Delete buttons
  observeEvent(input$delete_row, {
    idx <- as.integer(input$delete_row)
    if (is.na(idx)) return(NULL)
    
    cur <- overview()
    if (idx >= 1 && idx <= nrow(cur)) {
      overview(cur[-idx, , drop = FALSE])
      download_msgs(character(0))
    }
  }, ignoreInit = TRUE)
  
  
  # Clear overview
  observeEvent(input$clear_overview, {
    overview(overview()[0, ])
    download_msgs(character(0))
  })
  
  # Retrieve & save
  retrieve_and_save <- function(zipfile = NULL, workdir = NULL) {
    ov <- overview()
    req(nrow(ov) > 0)
    
    if (is.null(workdir)) workdir <- file.path(tempdir(), "export")
    if (dir.exists(workdir)) unlink(workdir, recursive = TRUE)
    dir.create(workdir, recursive = TRUE)
    
    download_msgs(character(0))
    local_msgs <- character(0)
    results <- list()
    
    withProgress(message = "Retrieving datasets...", value = 0, {
      for (i in seq_len(nrow(ov))) {
        ds <- ov$dataset[i]
        year <- ov$year[i]
        outfile <- file.path(workdir, paste0(ds, "_", i, ".geojson"))
        
        tryCatch({
          poly_sf <- ov$polygon_sf[[i]]
          mypolygon <- poly_sf$wkt[1]
          
          # build request URL for non-NDVI / non-Weather datasets
          myurl <- switch(
            ds,
            "Agricultural fields" = ndc_url("Fields", params = c(geometry = mypolygon, epsg = "4326", year = year, output_epsg = "4326")),
            "AHN" = ndc_url("AHN", params = c(geometry = mypolygon, epsg = "4326")),
            "Soil map" = ndc_url("Soiltypes", params = c(geometry = mypolygon, epsg = "4326", output_epsg = "4326", page_size = "25", page_offset = "0")),
            "Weather" = NULL,
            "NDVI" = NULL
          )
          
          
          # --- NDVI ---
          # --- NDVI ---
          if (ds == "NDVI") {
            if (input$ndvi_mode == "single") {
              year  <- as.integer(input$ndvi_year)
              month <- as.integer(input$ndvi_month)
              r <- download_avg_ndvi_month(poly_sf, year, month)
              
              if (!is.null(r)) {
                outfile <- file.path(workdir, paste0("NDVI_", year, "_", sprintf("%02d", month), ".tif"))
                terra::writeRaster(r, outfile, overwrite = TRUE)
                results[[paste0("NDVI_", i)]] <- r
                local_msgs <- c(local_msgs, paste0("Retrieved: NDVI ", year, "-", month))
              } else {
                local_msgs <- c(local_msgs, paste0("Failed: NDVI ", year, "-", month))
              }
              
            } else {  # NDVI range
              start_date <- ov$date_from[i]
              end_date   <- ov$date_to[i]
              
              if (is.na(start_date) || is.na(end_date)) {
                local_msgs <- c(local_msgs, paste0("Failed: NDVI - invalid date range for row ", i))
                incProgress(1 / nrow(ov))
              } else {
                start_year  <- as.integer(format(start_date, "%Y"))
                start_month <- as.integer(format(start_date, "%m"))
                end_year    <- as.integer(format(end_date, "%Y"))
                end_month   <- as.integer(format(end_date, "%m"))
                
                r_stack <- tryCatch({
                  download_avg_ndvi_stack(
                    poly = poly_sf,
                    start_year = start_year,
                    start_month = start_month,
                    end_year = end_year,
                    end_month = end_month
                  )
                }, error = function(e) {
                  message("Error in download_avg_ndvi_stack(): ", e$message)
                  NULL
                })
                
                if (!is.null(r_stack) && terra::nlyr(r_stack) > 0) {
                  # optionally name layers
                  layer_dates <- seq(from = start_date, to = end_date, by = "month")
                  names(r_stack) <- paste0("NDVI_", format(layer_dates, "%Y%m"))
                  
                  outfile <- file.path(workdir, paste0("NDVI_", format(start_date, "%Y%m"), "_to_", format(end_date, "%Y%m"), ".tif"))
                  terra::writeRaster(r_stack, outfile, overwrite = TRUE)
                  results[[paste0("NDVI_", i)]] <- r_stack
                  local_msgs <- c(local_msgs, paste0("Retrieved: NDVI stack ", format(start_date, "%Y-%m"), " to ", format(end_date, "%Y-%m")))
                } else {
                  local_msgs <- c(local_msgs, paste0("Failed: NDVI - no data in range ", start_date, " to ", end_date))
                }
                incProgress(1 / nrow(ov))
              }
            }  # end NDVI range else
            next  # move to next dataset
          }  # end ds == "NDVI"
          
          
          # --- Weather ---
          if (ds == "Weather") {
            cen_res <- get_closest_meteostation(mypolygon, token = mytoken)
            closest_id <- cen_res$closest_id
            if (is.null(closest_id)) {
              local_msgs <- c(local_msgs, paste0("Failed: Weather - no nearby station found"))
              incProgress(1 / nrow(ov))
              next
            }
            df <- ov$date_from[i]
            dt <- ov$date_to[i]
            if (is.na(df) || is.na(dt)) {
              local_msgs <- c(local_msgs, paste0("Failed: Weather - date_from or date_to is NA"))
              incProgress(1 / nrow(ov))
              next
            }
            meteo_sf <- if (df == dt) {
              get_meteo_for_date(closest_id, df, mytoken)
            } else {
              get_meteo_for_long_period(
                meteostation = closest_id,
                fromdate = df,
                todate = dt,
                token = mytoken,
                by_days = 200,
                sleep_sec = 0.5
              )
            }
            if (is.null(meteo_sf) || nrow(meteo_sf) == 0) {
              local_msgs <- c(local_msgs, "Failed: Weather - no data returned")
              incProgress(1 / nrow(ov))
              next
            }
            st_write(meteo_sf, outfile, delete_dsn = TRUE)
            results[[paste0(ds, "_", i)]] <- meteo_sf
            local_msgs <- c(local_msgs, paste0("Retrieved: ", ds))
            incProgress(1 / nrow(ov))
            next
          }  # end ds == "Weather"
          
          # --- Other datasets (GeoJSON) ---
          myres <- content(VERB("GET", url = myurl, add_headers(myheaders)))
          myres_sf <- geojson_sf(toJSON(myres, auto_unbox = TRUE))
          names(myres_sf) <- gsub("[:/\\?<>\\|*\"\\\\]", "_", names(myres_sf))
          st_write(myres_sf, outfile, delete_dsn = TRUE)
          results[[paste0(ds, "_", i)]] <- myres_sf
          local_msgs <- c(local_msgs, paste0("Retrieved: ", ds))
          
        }, error = function(e) {
          local_msgs <- c(local_msgs, paste0("Failed: ", ds, " - ", e$message))
        })
        
        incProgress(1 / nrow(ov))
      }  # end for loop
      
    })
    
    if (!is.null(zipfile)) {
      zip::zipr(zipfile, list.files(workdir, full.names = TRUE))
      local_msgs <- c(local_msgs, paste0("Zip written to: ", zipfile))
    }
    
    download_msgs(c(download_msgs(), local_msgs))
    
    list(
      overview = ov,
      messages = download_msgs(),
      zipfile = zipfile,
      datasets = results
    )
  }
  
  output$download_ui <- renderUI({
    if (nrow(overview()) == 0) {
      div(style = "color: grey; font-style: italic;",
          "Download button will appear here once you add a dataset.")
    } else {
      div(style = "display:flex; gap:10px;",
          downloadButton("download_data", "Download dataset(s)", class = "btn-custom"),
          actionButton("return_to_r", "Return data to R (close app)", class = "btn-custom"))
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("naturedatacube_", Sys.Date(), ".zip"),
    content = function(zipfile) retrieve_and_save(zipfile)
  )
  
  output$download_messages <- renderUI({
    msgs <- download_msgs()
    if (length(msgs) == 0)
      p("Updates on dataset retrieval will appear here.", style = "color: grey; font-style: italic;")
    else
      HTML(paste(msgs, collapse = "<br>"))
  })
  
  observeEvent(input$return_to_r, {
    res <- retrieve_and_save()
    stopApp(res)
  })
  
}


# ============================================================
# Run app
# ============================================================

shinyApp(ui, server)
>>>>>>> 5303c5d (Add files via upload)
