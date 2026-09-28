# ==============================================================================
# ui_main.R
# Main UI definition for Weather Station Dashboard
# Defines the overall layout and tab structure
# ==============================================================================

library(shiny)
library(shinydashboard)
library(plotly)

# Source UI components
# Note: These are sourced by app.R before this file

# ------------------------------------------------------------------------------
#' Build Main Dashboard UI
#'
#' Constructs the complete Shiny dashboard UI with all tabs and components.
#'
#' @return A Shiny dashboardPage object
#' @export
# ------------------------------------------------------------------------------
build_dashboard_ui <- function() {
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$title("Weather Station Dashboard"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$style(HTML(custom_css())),
      tags$script(HTML("
        // Wait for sidebar to be ready, then set initial state
        $(document).on('shiny:connected', function(event) {
          // Initial state set by server observer
          // CSS will handle show/hide based on data-unified-active attribute
        });
      "))
    ),
    dashboardPage(
      skin = "blue",

      # ==========================================================================
      # HEADER
      # ==========================================================================
      dashboardHeader(
        title = span(icon("cloud-sun"), " Weather Station"),
        titleWidth = 280,

        # Right-side dropdown menus
        dropdownMenuOutput("alert_menu")
      ),

      # ==========================================================================
      # SIDEBAR
      # ==========================================================================
      dashboardSidebar(
        width = 280,
        sidebarMenu(
          id = "main_tabs",
          menuItem(
            "Weather Station",
            tabName = "weather_station",
            icon = icon("broadcast-tower"),
            selected = TRUE
          ),
          menuItem(
            "NOAA Data",
            tabName = "noaa_data",
            icon = icon("cloud")
          ),
          menuItem(
            "Forecast",
            tabName = "forecast",
            icon = icon("calendar-alt")
          ),
          menuItem(
            "Unified View",
            tabName = "unified",
            icon = icon("layer-group")
          ),
          hr(),

          # Station selector (for future multi-station support)
          div(
            style = "padding: 10px 15px;",
            selectInput(
              inputId = "station_selector",
              label = "Station",
              choices = NULL,
              # Populated by server
              width = "100%"
            )
          ),
          hr(),

          # Controls in sidebar
          div(
            id = "dashboard_controls_standard",
            dashboard_controls(time_windows)
          ),
          hr(),

          # Unified View Controls (shown when on Unified tab)
          div(
            id = "unified_controls_sidebar",
            style = "padding: 10px 15px;",
            h4("Unified Comparison", style = "margin-top: 0; font-weight: 600; font-size: 0.95rem;"),
            radioButtons(
              inputId = "unified_baseline",
              label = "Baseline",
              choices = c(
                "Compare to NOAA Obs" = "noaa_obs",
                "Compare to Tempest" = "tempest"
              ),
              selected = "noaa_obs",
              inline = FALSE
            ),
            selectInput(
              inputId = "unified_variable",
              label = "Variable",
              choices = c(
                "Temperature" = "temperature",
                "Humidity" = "humidity",
                "Pressure" = "pressure",
                "Wind (Avg)" = "wind_avg",
                "Wind (Gust)" = "wind_gust"
              ),
              selected = "temperature"
            ),
            selectInput(
              inputId = "unified_noaa_source",
              label = "NOAA Source",
              choices = c("NWS" = "NWS"),
              selected = "NWS"
            ),
            uiOutput("unified_target_time_selector")
          ),
          hr(),

          # Timezone display
          div(
            style = "padding: 10px 15px; color: #b8c7ce; font-size: 0.85rem;",
            p(icon("clock"), " All times in Eastern (ET)"),
            uiOutput("current_time_display")
          )
        )
      ),

      # ==========================================================================
      # BODY
      # ==========================================================================
      dashboardBody(
        # Custom CSS
        tags$head(
          tags$style(HTML(custom_css())),
          tags$link(
            rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
          )
        ),
        tabItems(
          # ======================================================================
          # WEATHER STATION TAB
          # ======================================================================
          tabItem(
            tabName = "weather_station",

            # Alert banner (shown when conditions exceed thresholds)
            uiOutput("alert_banner"),

            # Current conditions value boxes
            h4(
              icon("thermometer-half"),
              " Current Conditions",
              style = "color: #495057; margin-bottom: 15px;"
            ),
            uiOutput("current_conditions"),
            br(),

            # Historical trends section
            fluidRow(
              column(
                width = 12,
                h4(
                  icon("chart-line"),
                  " Historical Trends",
                  style = "color: #495057; margin-bottom: 15px;"
                )
              )
            ),

            # Temperature and Humidity row
            fluidRow(
              shinydashboard::box(
                title = NULL,
                width = 6,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("temp_plot", height = "300px"),
                  type = 6,
                  color = "#E63946"
                )
              ),
              shinydashboard::box(
                title = NULL,
                width = 6,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("humidity_plot", height = "300px"),
                  type = 6,
                  color = "#457B9D"
                )
              )
            ),

            # Wind row
            fluidRow(
              shinydashboard::box(
                title = NULL,
                width = 8,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("wind_plot", height = "300px"),
                  type = 6,
                  color = "#2A9D8F"
                )
              ),
              shinydashboard::box(
                title = NULL,
                width = 4,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("wind_rose_plot", height = "350px"),
                  type = 6,
                  color = "#2A9D8F"
                )
              )
            ),

            # Precipitation and Pressure row
            fluidRow(
              shinydashboard::box(
                title = NULL,
                width = 6,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("precip_plot", height = "250px"),
                  type = 6,
                  color = "#1D3557"
                )
              ),
              shinydashboard::box(
                title = NULL,
                width = 6,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("pressure_plot", height = "250px"),
                  type = 6,
                  color = "#6C757D"
                )
              )
            ),

            # Lightning (only shown if there's activity)
            fluidRow(
              shinydashboard::box(
                title = NULL,
                width = 12,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                uiOutput("lightning_section")
              )
            ),

            # Station info footer
            fluidRow(
              column(
                width = 12,
                hr(),
                uiOutput("station_info")
              )
            )
          ),

          # ======================================================================
          # NOAA DATA TAB
          # ======================================================================
          tabItem(
            tabName = "noaa_data",
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("sliders-h"), "NOAA Controls"),
                width = 12,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                column(
                  width = 4,
                  selectInput(
                    inputId = "noaa_source_filter",
                    label = "Data Source",
                    choices = c("NWS" = "NWS", "All Sources" = "all"),
                    selected = "NWS"
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "noaa_variable",
                    label = "Variable",
                    choices = c(
                      "Temperature" = "temperature",
                      "Humidity" = "humidity",
                      "Pressure" = "pressure",
                      "Wind (Avg)" = "wind_avg",
                      "Wind (Gust)" = "wind_gust"
                    ),
                    selected = "temperature"
                  )
                ),
                column(
                  width = 4,
                  br(),
                  htmlOutput("noaa_data_status")
                )
              )
            ),
            fluidRow(
              uiOutput("noaa_summary_boxes")
            ),
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("chart-line"), "NOAA Hourly Observations"),
                width = 8,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("noaa_observation_plot", height = "330px"),
                  type = 6,
                  color = "#457B9D"
                )
              ),
              shinydashboard::box(
                title = tagList(icon("stream"), "NOAA Source Mix"),
                width = 4,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("noaa_source_mix_plot", height = "330px"),
                  type = 6,
                  color = "#2A9D8F"
                )
              )
            ),
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("balance-scale"), "NOAA vs Tempest (Same Variable)"),
                width = 12,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("noaa_vs_tempest_plot", height = "330px"),
                  type = 6,
                  color = "#E63946"
                )
              )
            )
          ),

          # ======================================================================
          # FORECAST TAB
          # ======================================================================
          tabItem(
            tabName = "forecast",
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("sliders-h"), "Forecast Controls"),
                width = 12,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                column(
                  width = 4,
                  selectInput(
                    inputId = "forecast_variable",
                    label = "Variable",
                    choices = c(
                      "Temperature" = "temperature",
                      "Humidity" = "humidity",
                      "Pressure" = "pressure",
                      "Wind (Avg)" = "wind_avg",
                      "Wind (Gust)" = "wind_gust"
                    ),
                    selected = "temperature"
                  )
                ),
                column(
                  width = 3,
                  selectInput(
                    inputId = "forecast_provider",
                    label = "Forecast Provider",
                    choices = c(
                      "NOAA" = "noaa",
                      "Open-Meteo" = "openmeteo",
                      "Blend (NOAA + Open-Meteo)" = "blend"
                    ),
                    selected = "blend"
                  )
                ),
                column(
                  width = 5,
                  sliderInput(
                    inputId = "forecast_horizon_hours",
                    label = "Forecast Horizon (hours)",
                    min = 12,
                    max = 240,
                    value = c(12, 168),
                    step = 6,
                    width = "100%"
                  )
                ),
                column(
                  width = 6,
                  textInput(
                    inputId = "forecast_map_location",
                    label = "Map Location (US city/state or ZIP)",
                    value = forecast_map_defaults$location_query,
                    placeholder = "e.g. Austin, TX or 27513"
                  )
                ),
                column(
                  width = 6,
                  selectInput(
                    inputId = "forecast_radar_layer",
                    label = "Map Layer",
                    choices = c(
                      "Radar" = "radar",
                      "Clouds" = "clouds",
                      "Satellite" = "satellite"
                    ),
                    selected = "radar"
                  )
                ),
                column(
                  width = 12,
                  htmlOutput("forecast_map_location_status")
                )
              )
            ),
            fluidRow(
              uiOutput("forecast_summary_boxes")
            ),
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("route"), "Latest Run Trajectory"),
                width = 8,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("forecast_latest_run_plot", height = "330px"),
                  type = 6,
                  color = "#2A9D8F"
                )
              ),
              shinydashboard::box(
                title = tagList(icon("align-left"), "Latest Weather Summary"),
                width = 4,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                tableOutput("forecast_summary_table")
              )
            ),
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("satellite-dish"), "Live Weather / Cloud Radar Map"),
                width = 12,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  uiOutput("forecast_radar_map"),
                  type = 6,
                  color = "#6C757D"
                )
              )
            ),
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("layer-group"), "Forecast Spread Across Runs"),
                width = 8,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("forecast_spread_plot", height = "330px"),
                  type = 6,
                  color = "#457B9D"
                )
              ),
              shinydashboard::box(
                title = tagList(icon("code-branch"), "Provider Comparison"),
                width = 4,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("forecast_provider_compare_plot", height = "330px"),
                  type = 6,
                  color = "#1D3557"
                )
              )
            ),
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("cloud-rain"), "Precipitation Probability (Latest Run)"),
                width = 12,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("forecast_precip_probability_plot", height = "300px"),
                  type = 6,
                  color = "#1D3557"
                )
              )
            )
          ),

          # ======================================================================
          # UNIFIED VIEW TAB
          # ======================================================================
          tabItem(
            tabName = "unified",
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("sliders-h"), "Forecast Lead Window"),
                width = 12,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                sliderInput(
                  inputId = "unified_lead_hours",
                  label = NULL,
                  min = 6,
                  max = 168,
                  value = c(6, 168),
                  step = 6,
                  width = "100%"
                )
              )
            ),
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("ruler-combined"), "NOAA vs Tempest (Hourly Observed)"),
                width = 12,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("unified_obs_delta_plot", height = "330px"),
                  type = 6,
                  color = "#457B9D"
                )
              )
            ),
            fluidRow(
              shinydashboard::box(
                title = tagList(icon("project-diagram"), "Forecast Evolution (Selected Target Hour)"),
                width = 6,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("unified_forecast_evolution_plot", height = "330px"),
                  type = 6,
                  color = "#2A9D8F"
                )
              ),
              shinydashboard::box(
                title = tagList(icon("bullseye"), "Forecast Accuracy by Lead Time"),
                width = 6,
                solidHeader = FALSE,
                status = NULL,
                style = "background-color: #FFFFFF;",
                withSpinner(
                  plotlyOutput("unified_accuracy_plot", height = "330px"),
                  type = 6,
                  color = "#F4A261"
                )
              )
            )
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
#' Custom CSS Styles
#'
#' Returns custom CSS for dashboard styling.
#'
#' @return Character string of CSS
# ------------------------------------------------------------------------------
custom_css <- function() {
  "
  /* Base typography */
  body {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    background-color: #F4F6F9;
  }

  /* Header styling */
  .main-header .logo {
    font-weight: 600;
    font-size: 1.1rem;
  }

  /* Content wrapper */
  .content-wrapper {
    background-color: #F4F6F9;
  }

  /* Box styling */
  .box {
    border-radius: 8px;
    border: none;
    box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  }

  .box-header {
    border-bottom: none;
  }

  /* Value box improvements */
  .weather-value-box {
    transition: transform 0.2s ease, box-shadow 0.2s ease;
  }

  .weather-value-box:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
  }

  /* Control panel */
  .dashboard-controls .form-group {
    margin-bottom: 0;
  }

  .dashboard-controls label {
    font-weight: 500;
    color: #495057;
  }

  /* Buttons */
  .btn-primary {
    background-color: #457B9D;
    border-color: #457B9D;
  }

  .btn-primary:hover {
    background-color: #3A6A8A;
    border-color: #3A6A8A;
  }

  .btn-secondary {
    background-color: #6C757D;
    border-color: #6C757D;
    color: #FFFFFF;
  }

  /* Alert styling */
  .alert-item {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    padding: 5px 12px;
    border-radius: 4px;
    font-size: 0.9rem;
  }

  .alert-item.alert-danger {
    background-color: rgba(220, 53, 69, 0.1);
  }

  .alert-item.alert-warning {
    background-color: rgba(255, 193, 7, 0.2);
  }

  /* Coming soon panel */
  .coming-soon-panel {
    transition: transform 0.2s ease;
  }

  .coming-soon-panel:hover {
    transform: scale(1.01);
  }

  /* Sidebar improvements */
  .sidebar-menu > li > a {
    font-weight: 500;
  }

  .sidebar-menu .badge {
    font-size: 0.7rem;
  }

  /* Plotly chart containers */
  .plotly {
    border-radius: 8px;
  }

  /* Spinner */
  .shiny-spinner-output-container {
    min-height: 200px;
  }

  /* Time display in sidebar */
  #current_time_display {
    font-size: 1.1rem;
    font-weight: 600;
    color: #FFFFFF;
  }

  /* Scrollbar styling */
  ::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }

  ::-webkit-scrollbar-track {
    background: #F1F1F1;
  }

  ::-webkit-scrollbar-thumb {
    background: #C1C1C1;
    border-radius: 4px;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: #A1A1A1;
  }

  /* Show/hide unified controls sidebar */
  #unified_controls_sidebar {
    display: none;
  }

  #dashboard_controls_standard {
    display: block;
  }

  .main-sidebar[data-unified-active='true'] #unified_controls_sidebar {
    display: block !important;
  }
  "
}
