# R shiny interface for AgroDataCube
# Created on  : 30 October 2025
# Last update : 19 December 2025

# Script to run the R Shiny interface for the AgroDataCube
# Version I made to test how Shiny works before working on the NatureDataCube interface

# Load packages ----------------------------------------------------------------

library(here)
library(leaflet)
library(ncdf4)
library(sf)
library(shiny)
library(tidyverse)
library(zip)

# AgroDataCube API key ---------------------------------------------------------

api_key <- "a27fe285-a1b7-449c-92cf-07e6b75bb6cb" # Your AgroDataCube API here to retrieve data, request here: https://agrodatacube.wur.nl/api/register.jsp

# Temporary "data" to test the app ---------------------------------------------

dataset_info <- read.csv(here::here("data/dataset_info_agrodatacube.csv"), header = TRUE, sep = ";")
dataset_info$start_date <- as.Date(dataset_info$start_date, format = "%d-%m-%Y")
dataset_info$end_date   <- as.Date(dataset_info$end_date,   format = "%d-%m-%Y")

# BRP map for a subset on the Veluwe
brp <- sf::read_sf(here::here('data/polygons/brp_subset_veluwe.shp'))
brp_4326 <- sf::st_transform(brp, 4326)

# Source functions
source(here::here("R/retrieval_functions/get_soil_types_for_field.R"))
source(here::here("R/retrieval_functions/get_ahn_for_field.R"))
source(here::here("R/retrieval_functions/get_ahn_image_for_field.R"))
source(here::here("R/retrieval_functions/get_meteostation_data.R"))
source(here::here("R/retrieval_functions/get_meteostations.R"))
source(here::here("R/retrieval_functions/get_ndvi_for_field.R"))
source(here::here("R/retrieval_functions/get_ndvi_image_for_field.R"))
source(here::here("R/retrieval_functions/get_nearest_meteostations_for_field.R"))
source(here::here("R/retrieval_functions/get_soil_code_details.R"))
source(here::here("R/retrieval_functions/get_soil_codes.R"))
source(here::here("R/retrieval_functions/get_soil_params_for_field.R"))

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(""),
  
  # Left panel------------------------------------------------------------------
  sidebarPanel(
    # 1. Select your area
    div(
      style = "margin-bottom: 10px;",
      p("1. Select an area on the map below by clicking:",
        style = "font-weight:bold;"),
      leafletOutput("map", height = "400px"),
      actionButton("zoom_poly", "Zoom to selected area", icon = icon("search"), style = "background-color: white; color: black;"), 
      br(), br(),
      
      # 2. Choose dataset(s)
      checkboxGroupInput("chosen_datasets", "2. Choose available dataset(s) for the selected area:", choices = NULL),
      uiOutput("no_chosen_datasets_text"), # Text that tells user that available datasets will be there if polygon has been clicked
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
    fluidRow( # Overview table (always visible)
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
  rv <- reactiveValues(data = list())
  fieldid <- reactiveVal(NULL)
  download_msgs <- reactiveVal(character(0))
  
  
  # Left panel -----------------------------------------------------------------
  #--------------------------------------------
  # Text when no upload and no selection of a polygon
  #--------------------------------------------
  output$no_chosen_datasets_text <- renderUI({
    if (is.null(clicked_polygon())) {
      tags$p(
        "Available dataset(s) will be shown here when a polygon has been clicked on the map above.",
        style = "color: grey; font-style: italic;")
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
      addPolygons(
        data = brp_4326,
        layerId = ~fid,
        color = "black",
        fillColor = "black",
        fillOpacity = 0.3,
        weight = 3,
      )
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
  # Map - clicking on a polygon
  #--------------------------------------------
  observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    req(event$id)
    
    poly <- brp_4326[brp_4326$fid == event$id, ]
    clicked_polygon(poly)
    fieldid(poly$fid) 
    
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
  # Map - zoom to clicked polygon
  #--------------------------------------------
  observeEvent(input$zoom_poly, {
    zoom_to_shape("map", clicked_polygon())
  })
  
  
  # Right panel ----------------------------------------------------------------
  #--------------------------------------------
  # Overview table - empty
  #--------------------------------------------
  empty_overview_row <- function(dataset, sub_dataset = NA,  start_date = NA, end_date = NA) {
    data.frame(
      dataset = dataset,
      sub_dataset = sub_dataset,
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
        desc_knmi <- dataset_info |>
          subset(dataset == "KNMI") |>
          (\(d) unique(d$description))() |>
          paste(collapse = "\n")
        desc_knmi_html <- gsub( # Automatically make from URLs clickable links
          "(https?://\\S+)", 
          '<a href="\\1" target="_blank">\\1</a>', 
          desc_knmi
        )
        knmi_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == "KNMI"])
        info_knmi <- dataset_info %>%
          dplyr::filter(dataset == "KNMI")
        tabPanel(title = ds, value = ds,
          br(),
          fluidRow(
            column(width =  6,
              tagList(
                h4("KNMI"),
                radioButtons(inputId = "sub_knmi", label = "Select subdataset:", choices = knmi_subs),
                dateRangeInput(
                  inputId = "knmi_date_range",
                  label = "From and until which date do you want data?",
                  start = min(info_knmi$start_date),
                  end = max(info_knmi$end_date),
                  min = min(info_knmi$start_date),
                  max = max(info_knmi$end_date),
                  format = "dd-mm-yyyy",
                  language = "eng",
                  weekstart = 1
                ),
                uiOutput("knmi_options"),
                actionButton(inputId = "add_knmi", label = "Add to overview table")
              )
            ),
            column(width = 6,
              wellPanel(
                h4(strong("Dataset information")),
                HTML(desc_knmi_html),  
                br(), br(),
                uiOutput("knmi_subdataset_info")
              )
            )
          ))
      } else if (ds == "Soil") {
        desc_soil <- dataset_info |>
          subset(dataset == "Soil") |>
          (\(d) unique(d$description))() |>
          paste(collapse = "\n")
        desc_soil_html <- gsub( # Automatically make from URLs clickable links
          "(https?://\\S+)", 
          '<a href="\\1" target="_blank">\\1</a>', 
          desc_soil)
        soil_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == "Soil"])
        tabPanel(title = ds, value = ds,
          br(),
          fluidRow(
            column(width =  6,
              tagList(
                h4("Soil"),
                radioButtons(inputId = "sub_soil", label = "Select subdataset:", choices = soil_subs),
                actionButton(inputId = "add_soil", label = "Add to overview table")
              )
            ),
            column(width = 6,
              wellPanel(
                h4(strong("Dataset information")),
                HTML(desc_soil_html),  
                br(), br(),
                uiOutput("soil_subdataset_info")
              )
            )
          ))
      } else if (ds == "NDVI") {
        desc_ndvi <- dataset_info |>
          subset(dataset == "NDVI") |>
          (\(d) unique(d$description))() |>
          paste(collapse = "\n")
        desc_ndvi_html <- gsub( # Automatically make from URLs clickable links
          "(https?://\\S+)", 
          '<a href="\\1" target="_blank">\\1</a>', 
          desc_ndvi
        )
        ndvi_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == "NDVI"])
        info_ndvi <- dataset_info %>%
          dplyr::filter(dataset == "NDVI")
        tabPanel(title = ds, value = ds,
          br(),
          fluidRow(
            column(width =  6,
              tagList(
                h4("NDVI"),
                radioButtons(inputId = "sub_ndvi", label = "Select subdataset:", choices = ndvi_subs),
                uiOutput("ndvi_options"),
                actionButton(inputId = "add_ndvi", label = "Add to overview table")
              )
            ),
            column(width = 6,
              wellPanel(
                h4(strong("Dataset information")),
                HTML(desc_ndvi_html),  
                br(), br(),
                uiOutput("ndvi_subdataset_info")
              )
            )
          ))  
      } else if (ds == "AHN") {
          desc_ahn <- dataset_info |>
            subset(dataset == "AHN") |>
            (\(d) unique(d$description))() |>
            paste(collapse = "\n")
          desc_ahn_html <- gsub( # Automatically make from URLs clickable links
            "(https?://\\S+)", 
            '<a href="\\1" target="_blank">\\1</a>', 
            desc_ahn
          )
          ahn_subs <- unique(dataset_info$sub_dataset[dataset_info$dataset == "AHN"])
          tabPanel(title = ds, value = ds,
            br(),
            fluidRow(
              column(width =  6,
                tagList(
                  h4("AHN"),
                  radioButtons(inputId = "sub_ahn", label = "Select subdataset:", choices = ahn_subs),
                  actionButton(inputId = "add_ahn", label = "Add to overview table")
                )
              ),
              column(width = 6,
                wellPanel(
                  h4(strong("Dataset information")),
                  HTML(desc_ahn_html),  
                  br(), br(),
                  uiOutput("ahn_subdataset_info")
                )
              )
            ))
      } else {
        tabPanel(
          title = ds,
          value = ds,
          tagList(
            h4(ds),
            p("Nothing here yet"),
            actionButton(
              inputId = paste0("add_", gsub(" ", "_", ds)),
              label = "Add to overview table"
            )
          )
        )
      }
    })
    do.call(tabsetPanel, c(id = "tabs", tabs))
  })
  
  #--------------------------------------------
  # Dataset tab: KNMI - sub dataset description
  #-------------------------------------------- 
  output$knmi_subdataset_info <- renderUI({
    req(input$sub_knmi)
    selected_info <- subset(dataset_info,
                            dataset == "KNMI" &
                              sub_dataset == input$sub_knmi)
    desc_sub <- paste(unique(selected_info$description_sub), collapse = "\n")
    desc_sub_html <- gsub( # Automatically make from URLs clickable links
      "(https?://\\S+)", 
      '<a href="\\1" target="_blank">\\1</a>', 
      desc_sub
    )
    HTML(paste0('<strong>Description:</strong><br>', desc_sub_html))
  })
  
  #--------------------------------------------
  # Dataset tab: Soil - sub dataset description
  #-------------------------------------------- 
  output$soil_subdataset_info <- renderUI({
    req(input$sub_soil)
    selected_info <- subset(dataset_info,
                            dataset == "Soil" &
                              sub_dataset == input$sub_soil)
    desc_sub <- paste(unique(selected_info$description_sub), collapse = "\n")
    desc_sub_html <- gsub( # Automatically make from URLs clickable links
      "(https?://\\S+)", 
      '<a href="\\1" target="_blank">\\1</a>', 
      desc_sub
    )
    HTML(paste0('<strong>Description:</strong><br>', desc_sub_html))
  })
  
  #--------------------------------------------
  # Dataset tab: NDVI - sub dataset description
  #-------------------------------------------- 
  output$ndvi_subdataset_info <- renderUI({
    req(input$sub_ndvi)
    selected_info <- subset(dataset_info,
                            dataset == "NDVI" &
                              sub_dataset == input$sub_ndvi)
    desc_sub <- paste(unique(selected_info$description_sub), collapse = "\n")
    desc_sub_html <- gsub( # Automatically make from URLs clickable links
      "(https?://\\S+)", 
      '<a href="\\1" target="_blank">\\1</a>', 
      desc_sub
    )
    HTML(paste0('<strong>Description:</strong><br>', desc_sub_html))
  })
  
  #--------------------------------------------
  # Dataset tab: AHN - sub dataset description
  #-------------------------------------------- 
  output$ahn_subdataset_info <- renderUI({
    req(input$sub_ahn)
    selected_info <- subset(dataset_info,
                            dataset == "AHN" &
                              sub_dataset == input$sub_ahn)
    desc_sub <- paste(unique(selected_info$description_sub), collapse = "\n")
    desc_sub_html <- gsub( # Automatically make from URLs clickable links
      "(https?://\\S+)", 
      '<a href="\\1" target="_blank">\\1</a>', 
      desc_sub
    )
    HTML(paste0('<strong>Description:</strong><br>', desc_sub_html))
  })
  
  #--------------------------------------------
  # Dataset tab: NDVI - options
  #-------------------------------------------- 
  output$ndvi_options <- renderUI({
    req(input$sub_ndvi)
    sub_ds_ndvi <- input$sub_ndvi
    info_ndvi <- dataset_info %>%
      dplyr::filter(dataset == "NDVI", sub_dataset == sub_ds_ndvi)
    if (sub_ds_ndvi == "NDVI") {
      tagList(
        dateRangeInput(
          inputId = "ndvi_date_range",
          label = "From and until which date do you want data?",
          start = min(info_ndvi$start_date),
          end = max(info_ndvi$end_date),
          min = min(info_ndvi$start_date),
          max = max(info_ndvi$end_date),
          format = "dd-mm-yyyy",
          language = "eng",
          weekstart = 1
        )
      )
    } else if (sub_ds_ndvi == "NDVI image") {
      tagList(
        dateInput(
          inputId = "ndvi_date",
          label = "For which date do you want data?",
          min = min(info_ndvi$start_date),
          max = max(info_ndvi$end_date),
          value = min(info_ndvi$start_date),
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
  # Overview table - add KNMI info 
  #-------------------------------------------- 
  observeEvent(input$add_knmi, {
    current <- overview()
    sub_val <- input$sub_knmi
    date_vals <- input$knmi_date_range
    if (is.null(sub_val) || is.null(date_vals) || length(date_vals) != 2) {
      showNotification(
        "Please fill in the subdataset before adding to overview table.",
        type = "error",
        duration = 4
      )
      return()
    }
    start_val_display <- format(as.Date(date_vals[1]), "%d-%m-%Y")
    end_val_display   <- format(as.Date(date_vals[2]), "%d-%m-%Y")
    exists <- any(
      current$dataset == "KNMI" &
        current$sub_dataset == sub_val &
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
    if (is.null(sub_val)) {
      showNotification(
        "Please fill in the subdataset before adding to overview table.",
        type = "error",
        duration = 4
      )
      return()
    }
    exists <- any(
      current$dataset == "Soil" &
        current$sub_dataset == sub_val 
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
    
    if (sub_val == "NDVI") {
      req(input$ndvi_date_range)
      start_val <- input$ndvi_date_range[1]
      end_val   <- input$ndvi_date_range[2]
      
    } else if (sub_val == "NDVI image") {
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
    
    start_val_fmt <- format(as.Date(start_val), "%d-%m-%Y")
    end_val_fmt   <- format(as.Date(end_val),   "%d-%m-%Y")
    exists <- any(
      current$dataset == "NDVI" &
        current$sub_dataset == sub_val &
        current$start_date == start_val_fmt &
        current$end_date == end_val_fmt
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
      start_date  = start_val_fmt,
      end_date    = end_val_fmt,
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
    if (is.null(sub_val)) {
      showNotification(
        "Please fill in the subdataset before adding to overview table.",
        type = "error",
        duration = 4
      )
      return()
    }
    exists <- any(
      current$dataset == "AHN" &
        current$sub_dataset == sub_val 
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
      HTML("<i>No dataset has been retrieved.</i>")
    } else {
      HTML(paste0(msgs, collapse = "<br>"))
    }
  })
  
  #--------------------------------------------
  # Retrieve and download
  #--------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() paste0("agrodatacube_data_", Sys.Date(), ".zip"),
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
          ds  <- ov$dataset[i]
          sub <- ov$sub_dataset[i]
          start <- as.Date(ov$start_date[i], format = "%d-%m-%Y")
          end   <- as.Date(ov$end_date[i],   format = "%d-%m-%Y")
          safe_sub <- gsub("[^A-Za-z0-9_]+", "_", sub)
          
          # KNMI
          if (ds == "KNMI") {
            outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_row_", i, ".csv"))
            write_fun <- function(data) write.csv(data, outfile, row.names = FALSE)
            
            closest_station <- get_nearest_meteostations_for_field(
              fieldid = fieldid(), 
              api_key = api_key)
            
            if (is.null(closest_station) || nrow(closest_station) == 0) {
              download_msgs(c(download_msgs(), paste0("No nearest KNMI station found for row ", i)))
              next
            }
            
            knmi_data <- get_meteostation_data(
              meteostationid = closest_station$meteostationid[1],
              fromdate = format(start, "%Y%m%d"),
              todate = format(end, "%Y%m%d"),
              api_key = api_key
            )
            
            if ("geometry" %in% colnames(knmi_data)) knmi_data$geometry <- NULL
            write_fun(knmi_data)
            
            # Soil
          } else if (ds == "Soil") {
            outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_row_", i, ".gpkg"))
            write_fun <- function(data) sf::st_write(data, outfile, delete_dsn = TRUE)
            
            if (sub == "Params") {
              soil_data <- get_soil_params_for_field(
                fieldid = fieldid(), 
                api_key = api_key)
            } else if (sub == "Types") {
              soil_data <- get_soil_types_for_field(
                fieldid = fieldid(), 
                api_key = api_key)
            }
            
            write_fun(soil_data)
            
          } else if (ds == "NDVI") {
            if (sub == "NDVI") {
              ndvi_data <- get_ndvi_for_field(
                fieldid = fieldid(), 
                fromdate = format(start, "%Y%m%d"), 
                todate = format(end, "%Y%m%d"), 
                api_key = api_key
                )
              outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_row_", i, ".csv"))
              if ("geometry" %in% colnames(ndvi_data)) ndvi_data$geometry <- NULL
              write.csv(ndvi_data, outfile, row.names = FALSE)
            } else if (sub == "NDVI image") {
             ndvi_data <- get_ndvi_image_for_field(
                fieldid = fieldid(), 
                date = format(start, "%Y%m%d"), 
                api_key = api_key
                )
              outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_row_", i, ".tif"))
              terra::writeRaster(ndvi_data, outfile, overwrite = TRUE)
            }
          } else if (ds == "AHN") {
            if (sub == "Zonal statistics") {
              ahn_data <- get_ahn_for_field(
                fieldid = fieldid(), 
                api_key = api_key
                )
              outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_row_", i, ".csv"))
              if ("geometry" %in% colnames(ahn_data)) ahn_data$geometry <- NULL
              write.csv(ahn_data, outfile, row.names = FALSE)
            } else if (ds == "AHN image") {
              ahn_data <- get_ahn_image_for_field(
                fieldid = fieldid(),
                api_key = api_key
                )
              outfile <- file.path(workdir, paste0(ds, "_", safe_sub, "_row_", i, ".tif"))
              terra::writeRaster(ahn_data, outfile, overwrite = TRUE)
            }

          }

          download_msgs(c(download_msgs(), paste0("Finished retrieving: ", ds, " - ", sub, " (row ", i, ")")))
          incProgress(1/n, detail = paste(ds, "-", sub, "row", i))
          
        }  
        
      })  
      
      download_msgs(c(download_msgs(), "<b>All datasets retrieved!</b>"))
      
      # Create ZIP
      oldwd <- setwd(workdir)
      on.exit(setwd(oldwd))
      zip(zipfile, files = list.files(workdir))
    }
  )
  

  
  
}



# ------------------------------------------------------------------------------

shinyApp(ui, server)




