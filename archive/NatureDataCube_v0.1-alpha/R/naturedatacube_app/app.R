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
    )
  )
)


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


