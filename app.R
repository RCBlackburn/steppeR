#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(shiny)
library(rgee)
library(sf)
library(dplyr)
library(plotly)
library(readr)
library(RColorBrewer)
source("utils.R")


# 1. Set rgee ----------------------------------------------------------
if (!reticulate::virtualenv_exists("rgee_py")) {
    set_rgee_dependencies()
}


# 2. Run again ee_Initialize, after a long period of inactivity. ----------
tryCatch(
    expr = ee$Image(0),
    error = function(e) {ee_Initialize()}
)




# 3. DEFINE HERE YOUR APP -------------------------------------------------

# ── Load summary stats ──
stats_long <- read_csv("data/soum_summary_wAOA.csv", show_col_types = FALSE)
# Height data is now already in centimeters from the cm layers, no conversion needed
years <- sort(unique(stats_long$year))

# ── Earth Engine FeatureCollection ──
soums_fc <- ee$FeatureCollection("projects/ee-rcb326/assets/steppeR/soum")

# ── Load soums geometry for highlighting (one-time load) ──
soums_geom <- NULL
tryCatch({
  soums_data <- soums_fc$getInfo()
  if (!is.null(soums_data) && !is.null(soums_data$features)) {
    soums_geom <- soums_data$features
  }
}, error = function(e) {
  message("Could not load soums geometry for highlighting")
})


# ── Helper Functions ──
asset_id <- function(mode, v, y) {
  if (mode == "Single Year") {
    # Use cm layers for height data
    if (v == "ht") {
      sprintf("projects/ee-rcb326/assets/steppeR/%s_%s_cm", v, y)
    } else {
      sprintf("projects/ee-rcb326/assets/steppeR/%s_%s", v, y)
    }
  } else {
    # Use cm slope layer for height data
    if (v == "ht") {
      sprintf("projects/ee-rcb326/assets/steppeR/slope_%s_cm", v)
    } else {
      sprintf("projects/ee-rcb326/assets/steppeR/slope_%s", v)
    }
  }
}

band_range <- function(img) {
  result <- img$reduceRegion(
    reducer = ee$Reducer$percentile(list(2, 98)),
    geometry = soums_fc$geometry(),
    bestEffort = TRUE,
    maxPixels = 1e7
  )$getInfo()

  # Extract values from the result
  if (length(result) > 0) {
    values <- unlist(result)
    return(c(min(values, na.rm = TRUE), max(values, na.rm = TRUE)))
  } else {
    return(c(0, 1))  # fallback values
  }
}

# ── UI Setup ──
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        margin-bottom: 15px;
      }
      .main-map {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      h4 {
        color: #495057;
        margin-top: 0;
      }
      .compact-select {
        margin-bottom: 10px;
      }
      .compact-select .form-group {
        margin-bottom: 5px;
      }
      .info-btn {
        border-radius: 50%;
        width: 30px;
        height: 30px;
        padding: 0;
        font-weight: bold;
        font-size: 14px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
      }
    "))
  ),
  titlePanel("steppeR – Steppe structure 2019-2024"),
  fluidRow(
    column(4,
           wellPanel(
             fluidRow(
               column(3,
                      radioButtons("mode", "Timespan", c("Single Year", "Multi-year"))
               ),
               column(3,
                      radioButtons("analysis_mode", "Region",
                                   choices = c("Soum" = "soum", "Custom" = "area"),
                                   selected = "soum")
               ),
               column(3,
                      selectInput("var", "Variable",
                                  choices = c("Cover" = "cover", "Height" = "ht", "Volume" = "vol"))
               ),
               column(3,
                      conditionalPanel(
                        condition = "input.mode=='Single Year'",
                        selectInput("yr", "Year", years, selected = max(years))
                      )
               )
             ),
             conditionalPanel(
               condition = "input.mode=='Single Year'",
               checkboxInput("mask_on", "Show area outside AOA (Area of Applicability)", FALSE)
             ),
             hr(),

             # Soum analysis instructions
             conditionalPanel(
               condition = "input.analysis_mode == 'soum'",
               p("Click on any soum boundary to view pre-computed statistics and trends.")
             ),

             # Area analysis controls
             conditionalPanel(
               condition = "input.analysis_mode == 'area'",
               p("Click multiple points on the map to draw a polygon for summary:"),
               div(style = "margin-bottom: 10px;",
                   actionButton("start_area_select", "Start Drawing Polygon", class = "btn-info")
               ),
               conditionalPanel(
                 condition = "output.area_selected",
                 br(),
                 verbatimTextOutput("selected_coords"),
                 br(),
                 actionButton("clear_area", "Clear Selection", class = "btn-warning")
               )
             ),
             br(),
             actionButton("refresh_map", "Refresh Map", class = "btn-primary"),
             actionButton("show_about", "i",
                          class = "btn-info btn-sm info-btn",
                          title = "About steppeR",
                          style = "margin-left: 10px;"),
             conditionalPanel(
               condition = "output.selTxt != 'Click on a soum boundary to select and view statistics'",
               br(),
               actionButton("clear_selection", "Clear Selection", class = "btn-secondary")
             ),
             hr(),
             verbatimTextOutput("selTxt")
           ),
           # Area Analysis Results
           conditionalPanel(
             condition = "input.analysis_mode == 'area' && input.mode == 'Single Year' && output.area_selected && output.area_stats != ''",
             wellPanel(
               h4("Selected Area Statistics"),
               tableOutput("area_stats_table")
             )
           ),
           conditionalPanel(
             condition = "input.analysis_mode == 'area' && input.mode == 'Multi-year' && output.area_selected && output.area_stats != ''",
             wellPanel(
               h4("Selected Area Multi-year Trends"),
               plotlyOutput("area_trend", height = "400px")
             )
           ),
           # Soum Analysis Results
           conditionalPanel(
             condition = "input.analysis_mode == 'soum' && input.mode == 'Single Year' && output.selTxt != 'Click on a soum boundary to select and view statistics'",
             wellPanel(
               h4("Soum Summary Statistics"),
               tableOutput("summary_table")
             )
           ),
           conditionalPanel(
             condition = "input.analysis_mode == 'soum' && input.mode == 'Multi-year' && output.selTxt != 'Click on a soum boundary to select and view statistics'",
             wellPanel(
               h4("Soum Vegetation Trends"),
               plotlyOutput("trend", height = "400px")
             )
           )
    ),
    column(8,
           div(class = "main-map",
               leafletOutput("map", height = "90vh")
           )
    )
  ),

  # About Modal Dialog
  tags$div(id = "about-modal")
)

# ── Server Logic ──
server <- function(input, output, session) {

  # Reactive Image based on mode, variable and year
  full_img <- reactive({
    req(input$var)
    if (input$mode == "Single Year") {
      req(input$yr)
      ee$Image(asset_id(input$mode, input$var, input$yr))
    } else {
      ee$Image(asset_id(input$mode, input$var, NULL))
    }
  })

  # Visualized Image (single-year select or full image for time series)
  vis_img <- reactive({
    req(full_img())
    full_img()$select(0)
  })

  # Mask for exclusion
  mask_img <- reactive({
    if (input$mode == "Single Year" && input$mask_on) {
      req(full_img())
      # Get the mask band directly - it should be 1s and 0s
      # Where 0 = excluded areas (outside AOA), 1 = included areas (inside AOA)
      full_img()$select(1)$eq(0)$selfMask()
    } else {
      NULL
    }
  })

  # Selected Soum
  sel <- reactiveVal(NULL)

  # Area selection functionality
  area_selection_mode <- reactiveVal(FALSE)
  selected_points <- reactiveVal(list())
  selected_polygon <- reactiveVal(NULL)

  # Track refresh map clicks to show description
  refresh_clicked <- reactiveVal(FALSE)

  # Show about modal on startup
  observe({
    showModal(modalDialog(
      title = "About steppeR",
      div(
        h5("Welcome to steppeR: Explore Mongolia's Grasslands, One Pixel at a Time"),
        p("steppeR lets you step through time and space to explore vegetation structure across central Mongolian aimags Tuv, Dundgovi, and Govisumber between 2019 and 2024. Using a cross-scale modeling approach with drones and Sentinel-2 satellite imagery (Blackburn et al. 2025, in review), this app brings central Mongolia's dynamic steppe landscapes to your fingertips."),

        # Table format for options
        tags$table(class = "table table-striped table-sm", style = "font-size: 12px;",
                   tags$thead(
                     tags$tr(
                       tags$th("Control", style = "width: 20%;"),
                       tags$th("Options", style = "width: 80%;")
                     )
                   ),
                   tags$tbody(
                     tags$tr(
                       tags$td(strong("Region")),
                       tags$td(
                         tags$div("• Soum: View pre-computed statistics for administrative boundaries"),
                         tags$div("• Custom: Draw custom areas and extract data directly from vegetation layers")
                       )
                     ),
                     tags$tr(
                       tags$td(strong("Timespan")),
                       tags$td(
                         tags$div("• Single Year: View vegetation data for a specific year"),
                         tags$div("• Multi-year: View average annual rate of change over the time period (2019-2024)")
                       )
                     ),
                     tags$tr(
                       tags$td(strong("Variable")),
                       tags$td(
                         tags$div("• Cover: Vegetation cover percentage (%)"),
                         tags$div("• Height: Vegetation height (cm)"),
                         tags$div("• Volume: Vegetation volume (m³)")
                       )
                     ),
                     tags$tr(
                       tags$td(strong("Year")),
                       tags$td("• Select a year between 2019 and 2024 for single year timespan")
                     ),
                     tags$tr(
                       tags$td(strong("Area of Applicability (AOA)")),
                       tags$td("• Areas where model predictions are most reliable based on training data coverage. Enable the AOA checkbox to see areas outside the reliable prediction zone for individual years.")
                     )
                   )
        ),

        h5(strong("How to Use")),
        tags$ul(
          tags$li(strong("Soum Analysis:"), " Click on any soum boundary to view pre-computed statistics and trends"),
          tags$li(strong("Custom Area Analysis:"), " Switch to Custom region mode, then:"),
          tags$ol(
            tags$li("Click 'Start Drawing Polygon' to begin"),
            tags$li("Click multiple points on the map to draw a polygon (minimum 3 points)"),
            tags$li("Click 'Stop Drawing' when you've drawn your desired shape"),
            tags$li("Data summarization will automatically begin once the polygon is completed"),
            tags$li("View statistics (Single Year) or time series plots (Multi-year)")
          )
        ),

        h5(strong("Data")),
        p("Soum statistics are pre-computed from vegetation layers with pixels outside AOA not included. Custom area statistics are calculated directly from the vegetation layers, providing summary information for your selected area. Data layers have a 20 x 20 meter spatial resolution and are available for download through figshare (link).")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Show about modal when info button is clicked
  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = "About steppeR",
      div(
        h5("Welcome to steppeR: Explore Mongolia's Grasslands, One Pixel at a Time"),
        p("steppeR lets you step through time and space to explore vegetation structure across central Mongolian aimags Tuv, Dundgovi, and Govisumber between 2019 and 2024. Using a cross-scale modeling approach with drones and Sentinel-2 satellite imagery (Blackburn et al. 2025, in review), this app brings central Mongolia's dynamic steppe landscapes to your fingertips."),

        # Table format for options
        tags$table(class = "table table-striped table-sm", style = "font-size: 12px;",
                   tags$thead(
                     tags$tr(
                       tags$th("Control", style = "width: 20%;"),
                       tags$th("Options", style = "width: 80%;")
                     )
                   ),
                   tags$tbody(
                     tags$tr(
                       tags$td(strong("Region")),
                       tags$td(
                         tags$div("• Soum: View pre-computed statistics for administrative boundaries"),
                         tags$div("• Custom: Draw custom areas and extract data directly from vegetation layers")
                       )
                     ),
                     tags$tr(
                       tags$td(strong("Timespan")),
                       tags$td(
                         tags$div("• Single Year: View vegetation data for a specific year"),
                         tags$div("• Multi-year: View average annual rate of change over the time period (2019-2024)")
                       )
                     ),
                     tags$tr(
                       tags$td(strong("Variable")),
                       tags$td(
                         tags$div("• Cover: Vegetation cover percentage (%)"),
                         tags$div("• Height: Vegetation height (cm)"),
                         tags$div("• Volume: Vegetation volume (m³)")
                       )
                     ),
                     tags$tr(
                       tags$td(strong("Year")),
                       tags$td("• Select a year between 2019 and 2024 for single year timespan")
                     ),
                     tags$tr(
                       tags$td(strong("Area of Applicability (AOA)")),
                       tags$td("• Areas where model predictions are most reliable based on training data coverage. Enable the AOA checkbox to see areas outside the reliable prediction zone for individual years.")
                     )
                   )
        ),

        h5(strong("How to Use")),
        tags$ul(
          tags$li(strong("Soum Analysis:"), " Click on any soum boundary to view pre-computed statistics and trends"),
          tags$li(strong("Custom Area Analysis:"), " Switch to Custom region mode, then:"),
          tags$ol(
            tags$li("Click 'Start Drawing Polygon' to begin"),
            tags$li("Click multiple points on the map to draw a polygon (minimum 3 points)"),
            tags$li("Click 'Stop Drawing' when you've drawn your desired shape"),
            tags$li("Data summarization will automatically begin once the polygon is completed"),
            tags$li("View statistics (Single Year) or time series plots (Multi-year)")
          )
        ),

        h5(strong("Data")),
        p("Soum statistics are pre-computed from vegetation layers with pixels outside AOA not included. Custom area statistics are calculated directly from the vegetation layers, providing summary information for your selected area. Data layers have a 20 x 20 meter spatial resolution and are available for download through figshare (link).")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Reset refresh state when map is refreshed
  observeEvent(input$refresh_map, {
    refresh_clicked(TRUE)
    sel(NULL)  # Clear selection when refreshing
    selected_polygon(NULL)  # Clear selected area when refreshing
    selected_points(list())
    area_selection_mode(FALSE)
    updateActionButton(session, "start_area_select", label = "Start Drawing Polygon")
  })

  # Reset area selection when switching to soum analysis mode
  observeEvent(input$analysis_mode, {
    if (input$analysis_mode == "soum") {
      # Clear area selection when switching to soum mode
      selected_polygon(NULL)
      selected_points(list())
      area_selection_mode(FALSE)
      updateActionButton(session, "start_area_select", label = "Start Drawing Polygon")
      leafletProxy("map") %>%
        clearGroup("area_selection")
      # Clear area analysis results
      output$area_stats <- renderText({ "" })
      output$area_stats_table <- renderTable({ NULL })
      output$area_trend <- renderPlotly({ NULL })
    } else if (input$analysis_mode == "area") {
      # Clear soum selection when switching to area mode
      sel(NULL)
      leafletProxy("map") %>%
        clearGroup("highlighted")
    }
  })

  # Start area selection (only works in area analysis mode)
  observeEvent(input$start_area_select, {
    req(input$analysis_mode == "area")

    if (area_selection_mode()) {
      # Stop selection mode and automatically analyze if we have enough points
      area_selection_mode(FALSE)
      updateActionButton(session, "start_area_select", label = "Start Drawing Polygon")

      current_points <- selected_points()
      if (length(current_points) >= 3) {
        # Create polygon from points
        coords <- do.call(rbind, lapply(current_points, function(p) c(p$lng, p$lat)))

        # Close the polygon by adding the first point at the end
        coords <- rbind(coords, coords[1,])

        selected_polygon(list(coordinates = coords))

        # Draw polygon on map
        leafletProxy("map") %>%
          clearGroup("area_selection") %>%
          clearGroup("polygon_preview") %>%
          addPolygons(
            lng = coords[,1],
            lat = coords[,2],
            group = "area_selection",
            fillColor = "blue",
            fillOpacity = 0.2,
            color = "blue",
            weight = 2
          )

        showNotification("Polygon completed! Analyzing area...", duration = 3)

        # Automatically trigger analysis
        analyze_selected_area()

      } else {
        showNotification("Need at least 3 points to create a polygon", type = "warning", duration = 3)
        # Clear incomplete selection
        selected_points(list())
        selected_polygon(NULL)
        leafletProxy("map") %>%
          clearGroup("area_selection") %>%
          clearGroup("polygon_preview")
      }
    } else {
      # Start selection mode
      area_selection_mode(TRUE)
      selected_points(list())
      selected_polygon(NULL)
      updateActionButton(session, "start_area_select", label = "Stop Drawing")
      showNotification("Click points on the map to draw a polygon. Click 'Stop Drawing' when done (minimum 3 points).", duration = 7)
      # Clear any existing area selection from map
      leafletProxy("map") %>%
        clearGroup("area_selection") %>%
        clearGroup("polygon_preview")
      # Clear previous results
      output$area_stats <- renderText({ "" })
      output$area_stats_table <- renderTable({ NULL })
      output$area_trend <- renderPlotly({ NULL })
    }
  })

  # Function to analyze selected area
  analyze_selected_area <- function() {
    req(selected_polygon())

    withProgress(message = 'Analyzing selected polygon...', value = 0, {
      tryCatch({
        # Convert polygon to Earth Engine geometry
        coords <- selected_polygon()$coordinates
        # Earth Engine expects coordinates in [lng, lat] format as nested list
        ee_coords <- lapply(1:nrow(coords), function(i) list(coords[i,1], coords[i,2]))
        polygon_geom <- ee$Geometry$Polygon(list(ee_coords))

        incProgress(0.3, detail = "Processing geometry...")

        if (input$mode == "Single Year") {
          # Single year analysis - get all three variables
          incProgress(0.3, detail = "Calculating statistics for all variables...")

          # Get images for all three variables
          cover_img <- ee$Image(asset_id("Single Year", "cover", input$yr))$select(0)
          ht_img <- ee$Image(asset_id("Single Year", "ht", input$yr))$select(0)
          vol_img <- ee$Image(asset_id("Single Year", "vol", input$yr))$select(0)

          # Calculate stats for each variable
          cover_result <- cover_img$reduceRegion(
            reducer = ee$Reducer$mean()$combine(ee$Reducer$stdDev(), "", TRUE),
            geometry = polygon_geom,
            scale = 30,
            bestEffort = TRUE,
            maxPixels = 1e7
          )$getInfo()

          ht_result <- ht_img$reduceRegion(
            reducer = ee$Reducer$mean()$combine(ee$Reducer$stdDev(), "", TRUE),
            geometry = polygon_geom,
            scale = 30,
            bestEffort = TRUE,
            maxPixels = 1e7
          )$getInfo()

          vol_result <- vol_img$reduceRegion(
            reducer = ee$Reducer$mean()$combine(ee$Reducer$stdDev(), "", TRUE),
            geometry = polygon_geom,
            scale = 30,
            bestEffort = TRUE,
            maxPixels = 1e7
          )$getInfo()

          incProgress(0.3, detail = "Formatting results...")

          if (!is.null(cover_result) && !is.null(ht_result) && !is.null(vol_result) &&
              length(cover_result) > 0 && length(ht_result) > 0 && length(vol_result) > 0) {

            # Extract values (height data is already in cm from cm layers)
            cover_mean <- cover_result[[names(cover_result)[1]]]
            cover_sd <- cover_result[[names(cover_result)[2]]]

            ht_mean <- ht_result[[names(ht_result)[1]]]  # Already in cm
            ht_sd <- ht_result[[names(ht_result)[2]]]    # Already in cm

            vol_mean <- vol_result[[names(vol_result)[1]]]
            vol_sd <- vol_result[[names(vol_result)[2]]]

            # Create comprehensive table similar to soum analysis
            area_stats_table <- data.frame(
              Variable = c("Cover", "Height", "Volume"),
              Mean = c(round(cover_mean, 3), round(ht_mean, 3), round(vol_mean, 3)),
              `Standard Deviation` = c(round(cover_sd, 3), round(ht_sd, 3), round(vol_sd, 3)),
              Units = c("%", "cm", "m³/ha"),
              check.names = FALSE
            )

            output$area_stats_table <- renderTable({
              area_stats_table
            }, striped = TRUE, hover = TRUE, bordered = TRUE)

            # Calculate approximate area (rough estimate)
            coords_deg <- selected_polygon()$coordinates
            area_estimate <- round(abs((max(coords_deg[,1]) - min(coords_deg[,1])) *
                                         (max(coords_deg[,2]) - min(coords_deg[,2]))), 4)

            # Update text output with summary
            stats_text <- paste0(
              "Selected Polygon Analysis - ", input$yr, "\n",
              "Cover: ", round(cover_mean, 3), "% (±", round(cover_sd, 3), ")\n",
              "Height: ", round(ht_mean, 3), " cm (±", round(ht_sd, 3), ")\n",
              "Volume: ", round(vol_mean, 3), " m³/ha (±", round(vol_sd, 3), ")\n",
              "Points: ", nrow(coords_deg), " | Est. Area: ", area_estimate, " deg²"
            )

            output$area_stats <- renderText({ stats_text })
          } else {
            output$area_stats <- renderText({ "No data available for selected polygon" })
            output$area_stats_table <- renderTable({
              data.frame(
                Variable = "No data available",
                Mean = NA,
                `Standard Deviation` = NA,
                Units = NA,
                check.names = FALSE
              )
            })
          }

        } else {
          # Time series analysis - get all three variables
          incProgress(0.1, detail = "Analyzing time series for all variables...")

          # Get data for all years and all variables
          cover_data <- list()
          ht_data <- list()
          vol_data <- list()

          for (year in years) {
            incProgress(0.4/length(years), detail = paste("Processing", year, "..."))

            # Get images for all three variables for this year
            cover_img <- ee$Image(asset_id("Single Year", "cover", year))$select(0)
            ht_img_raw <- ee$Image(asset_id("Single Year", "ht", year))$select(0)
            vol_img <- ee$Image(asset_id("Single Year", "vol", year))$select(0)

            # Height images from cm layers are already in cm, no conversion needed
            ht_img <- ht_img_raw

            # Calculate mean for each variable
            cover_result <- cover_img$reduceRegion(
              reducer = ee$Reducer$mean(),
              geometry = polygon_geom,
              scale = 30,
              bestEffort = TRUE,
              maxPixels = 1e7
            )$getInfo()

            ht_result <- ht_img$reduceRegion(
              reducer = ee$Reducer$mean(),
              geometry = polygon_geom,
              scale = 30,
              bestEffort = TRUE,
              maxPixels = 1e7
            )$getInfo()

            vol_result <- vol_img$reduceRegion(
              reducer = ee$Reducer$mean(),
              geometry = polygon_geom,
              scale = 30,
              bestEffort = TRUE,
              maxPixels = 1e7
            )$getInfo()

            # Store results if available
            if (!is.null(cover_result) && length(cover_result) > 0) {
              cover_data[[as.character(year)]] <- cover_result[[names(cover_result)[1]]]
            }

            if (!is.null(ht_result) && length(ht_result) > 0) {
              ht_data[[as.character(year)]] <- ht_result[[names(ht_result)[1]]]  # Already in cm from cm layers
            }

            if (!is.null(vol_result) && length(vol_result) > 0) {
              vol_data[[as.character(year)]] <- vol_result[[names(vol_result)[1]]]
            }
          }

          incProgress(0.2, detail = "Creating plots...")

          if (length(cover_data) > 0 || length(ht_data) > 0 || length(vol_data) > 0) {

            # Create plotting function similar to soum analysis
            mk_area <- function(data_list, var_name, col) {
              if (length(data_list) == 0) return(NULL)

              df <- data.frame(
                year = as.numeric(names(data_list)),
                value = unlist(data_list)
              )

              if (nrow(df) < 2) return(NULL)

              # Add trend line
              fit <- tryCatch({
                lm(value ~ year, df)
              }, error = function(e) NULL)

              p <- plot_ly(df, x = ~year, y = ~value, type = "scatter",
                           mode = "lines+markers", name = var_name,
                           line = list(color = col), marker = list(color = col))

              if (!is.null(fit)) {
                preds <- data.frame(year = df$year, fitted = predict(fit))
                p <- p %>%
                  add_lines(data = preds, x = ~year, y = ~fitted,
                            line = list(color = col, dash = "dash"),
                            showlegend = FALSE)
              }

              return(p)
            }

            # Create plots for each variable (matching soum analysis colors)
            plots <- list(
              mk_area(cover_data, "Cover (%)", "#41ab5d"),    # Green from YlGn palette
              mk_area(ht_data, "Height (cm)", "#d73027"),     # Red from Spectral palette
              mk_area(vol_data, "Volume (m³/ha)", "#2c7fb8")  # Blue from YlGnBu palette
            )

            # Remove NULL plots
            plots <- plots[!sapply(plots, is.null)]

            if (length(plots) > 0) {
              combined_plot <- subplot(plots, nrows = length(plots), shareX = TRUE, margin = 0.05) %>%
                layout(
                  title = "Selected Polygon Multi-year Trends - All Variables",
                  font = list(size = 10),
                  margin = list(l = 50, r = 20, t = 40, b = 40)
                )

              output$area_trend <- renderPlotly({ combined_plot })
              output$area_stats <- renderText({ "" })  # Clear single year stats
            } else {
              output$area_trend <- renderPlotly({ NULL })
              output$area_stats <- renderText({ "No multi-year data available for selected polygon" })
            }
          } else {
            output$area_trend <- renderPlotly({ NULL })
            output$area_stats <- renderText({ "No multi-year data available for selected polygon" })
          }
        }

        incProgress(0.1, detail = "Complete!")
        showNotification("Polygon analysis complete!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Error analyzing polygon:", e$message), type = "error", duration = 5)
        output$area_stats <- renderText({ paste("Error:", e$message) })
      })
    })
  }

  # Handle map clicks for area selection (only in area analysis mode)
  observeEvent(input$map_click, {
    if (input$analysis_mode == "area" && area_selection_mode()) {
      click <- input$map_click
      current_points <- selected_points()

      # Add the clicked point
      current_points[[length(current_points) + 1]] <- list(lat = click$lat, lng = click$lng)
      selected_points(current_points)

      # Add marker to map
      leafletProxy("map") %>%
        addMarkers(
          lng = click$lng,
          lat = click$lat,
          group = "area_selection",
          popup = paste("Point", length(current_points))
        )

      # Draw lines between points if we have more than one point
      if (length(current_points) > 1) {
        # Get coordinates for line drawing
        coords <- do.call(rbind, lapply(current_points, function(p) c(p$lng, p$lat)))

        # Draw polygon preview (not closed yet)
        leafletProxy("map") %>%
          clearGroup("polygon_preview") %>%
          addPolylines(
            lng = coords[,1],
            lat = coords[,2],
            group = "polygon_preview",
            color = "blue",
            weight = 2,
            opacity = 0.7
          )

        # If we have 3+ points, also show a preview line back to the first point
        if (length(current_points) >= 3) {
          leafletProxy("map") %>%
            addPolylines(
              lng = c(coords[nrow(coords),1], coords[1,1]),
              lat = c(coords[nrow(coords),2], coords[1,2]),
              group = "polygon_preview",
              color = "blue",
              weight = 2,
              opacity = 0.4,
              dashArray = "5,5"
            )
        }
      }

      if (length(current_points) == 1) {
        showNotification("Click more points to draw polygon. Need at least 3 points.", duration = 4)
      } else if (length(current_points) >= 3) {
        showNotification(paste("Added point", length(current_points), "- Click 'Finish Polygon' when done"), duration = 3)
      } else {
        showNotification(paste("Added point", length(current_points), "- Need at least 3 points total"), duration = 3)
      }

    } else if (input$analysis_mode == "soum") {
      # Normal click handling for soum selection (only in soum analysis mode)
      click <- input$map_click
      if (!is.null(click)) {
        # Extract coordinates and query the feature collection
        point <- ee$Geometry$Point(list(click$lng, click$lat))
        tryCatch({
          # Query the feature collection at the clicked point
          feature <- soums_fc$filterBounds(point)$first()
          properties <- feature$getInfo()
          if (!is.null(properties) && !is.null(properties$properties$soum)) {
            soum_name <- properties$properties$soum
            sel(soum_name)
            showNotification(paste("Selected soum:", soum_name), duration = 3)
          }
        }, error = function(e) {
          # Silently ignore errors
        })
      }
    }
  })

  # Clear area selection
  observeEvent(input$clear_area, {
    selected_polygon(NULL)
    selected_points(list())
    area_selection_mode(FALSE)
    updateActionButton(session, "start_area_select", label = "Start Drawing Polygon")
    leafletProxy("map") %>%
      clearGroup("area_selection") %>%
      clearGroup("polygon_preview")
    # Clear analysis results
    output$area_stats <- renderText({ "" })
    output$area_stats_table <- renderTable({ NULL })
    output$area_trend <- renderPlotly({ NULL })
    showNotification("Polygon selection cleared", duration = 3)
  })

  # Create highlighted soum layer
  highlighted_soum <- reactive({
    if (!is.null(sel())) {
      soums_fc$filter(ee$Filter$eq("soum", sel()))
    } else {
      NULL
    }
  })

  # Create styled soums layer (outline only)
  styled_soums <- reactive({
    soums_fc$style(
      fillColor = "00000000",
      color = "000000",
      width = 2
    )
  })

  # Create styled highlighted soum layer
  styled_highlighted_soum <- reactive({
    if (!is.null(sel()) && !is.null(highlighted_soum())) {
      highlighted_soum()$style(
        fillColor = "00000000",
        color = "FF0000",
        width = 4
      )
    } else {
      NULL
    }
  })

  # Create Map with Earth Engine Layer
  output$map <- renderLeaflet({
    # Make reactive to inputs and refresh button, but NOT selection changes
    input$refresh_map
    input$mode
    input$var
    if (input$mode == "Single Year") input$yr
    input$mask_on
    req(vis_img())

    withProgress(message = 'Loading map...', value = 0, {
      # Get band range for visualization
      incProgress(0.3, detail = "Calculating band range...")
      rng <- tryCatch({
        band_range(vis_img())
      }, error = function(e) {
        showNotification(paste("Error getting band range:", e$message), type = "warning")
        c(0, 100)  # fallback range
      })

      incProgress(0.3, detail = "Setting up visualization...")
      # Set up palette and visualization parameters
      if (input$mode == "Single Year") {
        pal <- if (input$var == "cover") "YlGn"
        else if (input$var == "ht") "Spectral"
        else "YlGnBu"
        viz <- list(
          min = rng[1],
          max = rng[2],
          palette = if (input$var == "ht") rev(brewer.pal(9, pal)) else brewer.pal(9, pal)
        )
      } else {
        # Multi-year variables - use symmetric range around zero
        max_abs <- max(abs(rng[1]), abs(rng[2]))
        viz <- list(
          min = -max_abs,
          max = max_abs,
          palette = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#e0e0e0",
                      "#abd9e9", "#74add1", "#4575b4", "#313695")
        )
      }

      # Create map using rgee Map object
      incProgress(0.2, detail = "Creating map...")
      Map$setCenter(105, 46, 6)  # Center on Mongolia with moderate zoom

      # Add Earth Engine image layer
      incProgress(0.1, detail = "Adding image layer...")
      m <- Map$addLayer(
        eeObject = vis_img(),
        visParams = viz,
        name = "Image Layer"
      )

      # Add soums feature collection
      incProgress(0.05, detail = "Adding boundaries...")
      m <- m + Map$addLayer(
        eeObject = styled_soums(),
        name = "Soums"
      )

      # Add mask layer if enabled (AFTER soums so it appears on top)
      if (!is.null(mask_img())) {
        incProgress(0.05, detail = "Adding AOA layer...")
        m <- m + Map$addLayer(
          eeObject = mask_img(),
          visParams = list(
            palette = c("000000")
          ),
          name = "Outside AOA",
          shown = TRUE,
          opacity = 1.0
        )

        # Add legend for AOA
        m <- m + Map$addLegend(
          visParams = list(
            min = 1,
            max = 1,
            palette = c("000000")
          ),
          name = "Outside AOA",
          position = "topright"
        )
      }

      # Add legend for the main raster layer
      incProgress(0.02, detail = "Adding legend...")
      legend_title <- if (input$mode == "Single Year") {
        var_display_name <- case_when(
          input$var == "cover" ~ "Cover",
          input$var == "ht" ~ "Height",
          input$var == "vol" ~ "Volume",
          TRUE ~ input$var
        )
        paste(var_display_name, "values for", input$yr)
      } else {
        var_display_name <- case_when(
          input$var == "cover" ~ "cover",
          input$var == "ht" ~ "height",
          input$var == "vol" ~ "volume",
          TRUE ~ input$var
        )
        paste("Average rate of", var_display_name, "change")
      }

      # Get variable units for legend
      var_units <- case_when(
        input$var == "cover" ~ "%",
        input$var == "ht" ~ if(input$mode == "Single Year") "cm" else "cm/year",
        input$var == "vol" ~ if(input$mode == "Single Year") "m³/ha" else "m³/ha/year",
        TRUE ~ ""
      )

      # Create legend - use viz params directly
      m <- m + Map$addLegend(
        visParams = viz,
        name = paste0(legend_title, " (", var_units, ")"),
        position = "bottomright"
      )

      m
    })
  })

  # Handle selection highlighting separately to avoid map redraw/zoom issues
  observeEvent(sel(), {
    if (!is.null(sel()) && !is.null(soums_geom)) {
      # Find the selected soum in the geometry data
      selected_feature <- NULL
      for (feature in soums_geom) {
        if (!is.null(feature$properties$soum) && feature$properties$soum == sel()) {
          selected_feature <- feature
          break
        }
      }

      if (!is.null(selected_feature) && !is.null(selected_feature$geometry$coordinates)) {
        tryCatch({
          coords <- selected_feature$geometry$coordinates[[1]]
          # Convert coordinates to leaflet format (lat, lng)
          coords_matrix <- do.call(rbind, lapply(coords, function(x) c(x[[2]], x[[1]])))

          leafletProxy("map") %>%
            clearGroup("highlighted") %>%
            addPolygons(
              lng = coords_matrix[,2],
              lat = coords_matrix[,1],
              group = "highlighted",
              fillColor = "transparent",
              color = "red",
              weight = 4,
              fillOpacity = 0,
              opacity = 1
            )
        }, error = function(e) {
          message("Error highlighting soum: ", e$message)
        })
      }
    } else {
      # Clear highlighted layer
      leafletProxy("map") %>%
        clearGroup("highlighted")
    }
  })

  # Handle click event on soum to show selected soum info
  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    if (!is.null(id)) {
      sel(id)
      showNotification(paste("Selected soum:", id), duration = 3)
    }
  })

  # Clear selection button
  observeEvent(input$clear_selection, {
    sel(NULL)
    # Explicitly clear the highlighted layer
    leafletProxy("map") %>%
      clearGroup("highlighted")
    showNotification("Selection cleared", duration = 2)
  })

  output$selTxt <- renderText({
    if (input$analysis_mode == "soum") {
      if (is.null(sel())) {
        "Click on a soum boundary to select and view statistics"
      } else {
        paste("Selected Soum:", sel())
      }
    } else {
      ""  # No text output needed for custom area mode
    }
  })

  # Check if area is selected
  output$area_selected <- reactive({
    !is.null(selected_polygon())
  })
  outputOptions(output, "area_selected", suspendWhenHidden = FALSE)

  # Display selected coordinates
  output$selected_coords <- renderText({
    if (!is.null(selected_polygon())) {
      coords <- selected_polygon()$coordinates
      n_points <- nrow(coords) - 1  # Subtract 1 because last point repeats first
      bbox <- paste0("Bounds: ",
                     round(min(coords[,2]), 4), "°N to ", round(max(coords[,2]), 4), "°N, ",
                     round(min(coords[,1]), 4), "°E to ", round(max(coords[,1]), 4), "°E")
      paste0("Selected Polygon:\n",
             "Points: ", n_points, "\n",
             bbox)
    } else if (length(selected_points()) > 0) {
      pts <- selected_points()
      points_text <- paste(sapply(1:length(pts), function(i) {
        paste0("Point ", i, ": ", round(pts[[i]]$lat, 4), "°N, ", round(pts[[i]]$lng, 4), "°E")
      }), collapse = "\n")
      paste0("Drawing Polygon:\n", points_text, "\n",
             if (length(pts) >= 3) "Click 'Finish Polygon' when done" else paste("Need", 3 - length(pts), "more point(s)"))
    } else {
      ""
    }
  })

  # Area statistics output
  output$area_stats <- renderText({
    ""  # Will be populated when analysis is performed
  })

  # Area statistics table output
  output$area_stats_table <- renderTable({
    NULL  # Will be populated when analysis is performed
  })

  # Area trend plot
  output$area_trend <- renderPlotly({
    NULL  # Will be populated when analysis is performed
  })

  # Summary table for single year mode
  output$summary_table <- renderTable({
    req(sel(), input$yr)
    d <- stats_long %>%
      filter(soum == sel(), year == as.numeric(input$yr)) %>%
      select(var, mean, sd) %>%
      arrange(var)

    if (nrow(d) == 0) {
      data.frame(
        Variable = "No data available",
        Mean = NA,
        `Standard Deviation` = NA,
        Units = NA,
        check.names = FALSE
      )
    } else {
      d %>%
        mutate(
          mean = round(mean, 3),
          sd = round(sd, 3),
          units = case_when(
            var == "cover" ~ "%",
            var == "ht" ~ "cm",
            var == "vol" ~ "m³/pixel",
            TRUE ~ ""
          )
        ) %>%
        rename(
          Variable = var,
          Mean = mean,
          `Standard Deviation` = sd,
          Units = units
        ) %>%
        mutate(
          Variable = case_when(
            Variable == "cover" ~ "Cover",
            Variable == "ht" ~ "Height",
            Variable == "vol" ~ "Volume",
            TRUE ~ Variable
          )
        )
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Plot time series trend for selected soum
  output$trend <- renderPlotly({
    req(sel())
    d <- stats_long %>% filter(soum == sel(), var %in% c("cover", "ht", "vol"))
    if (nrow(d) == 0) return(NULL)

    mk <- function(v, col) {
      dv <- d %>% filter(var == v) %>% arrange(year)
      if (nrow(dv) < 2) return(NULL)

      fit <- tryCatch({
        lm(mean ~ year, dv)
      }, error = function(e) NULL)

      # Get variable display name and units
      var_display <- case_when(
        v == "cover" ~ "Cover (%)",
        v == "ht" ~ "Height (cm)",
        v == "vol" ~ "Volume (m³/ha)",
        TRUE ~ v
      )

      p <- plot_ly(dv, x = ~year, y = ~mean, type = "scatter", mode = "lines+markers",
                   name = var_display, line = list(color = col), marker = list(color = col))

      if (!is.null(fit)) {
        preds <- data.frame(year = dv$year, fitted = predict(fit))
        p <- p %>%
          add_lines(data = preds, x = ~year, y = ~fitted,
                    line = list(color = col, dash = "dash"),
                    showlegend = FALSE)  # Hide trend lines from legend
      }

      return(p)
    }

    plots <- list(
      mk("cover", "#41ab5d"),    # Green from YlGn palette
      mk("ht", "#d73027"),       # Red from Spectral palette (high values)
      mk("vol", "#2c7fb8")       # Blue from YlGnBu palette
    )

    plots <- plots[!sapply(plots, is.null)]

    if (length(plots) > 0) {
      subplot(plots, nrows = length(plots), shareX = TRUE, margin = 0.05) %>%
        layout(
          title = paste("Trends –", sel()),
          font = list(size = 10),
          margin = list(l = 50, r = 20, t = 40, b = 40)
        )
    } else {
      NULL
    }
  })
}

# Run the app
shinyApp(ui, server)

# -------------------------------------------------------------------------


