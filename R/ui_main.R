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
    tags$head(
      tags$title("Weather Station Dashboard"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
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
            icon = icon("cloud"),
            badgeLabel = "Soon",
            badgeColor = "yellow"
          ),
          menuItem(
            "Forecast",
            tabName = "forecast",
            icon = icon("calendar-alt"),
            badgeLabel = "Soon",
            badgeColor = "yellow"
          ),
          menuItem(
            "Unified View",
            tabName = "unified",
            icon = icon("layer-group"),
            badgeLabel = "Soon",
            badgeColor = "yellow"
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
          dashboard_controls(time_windows),
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
              box(
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
              box(
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
              box(
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
              box(
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
              box(
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
              box(
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
              box(
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
          # NOAA DATA TAB (Placeholder)
          # ======================================================================
          tabItem(
            tabName = "noaa_data",
            coming_soon_panel(
              title = "NOAA Data Integration",
              description = paste(
                "Integration with NOAA weather data is coming soon.",
                "This will include official observations, historical data,",
                "and climate normals for your area."
              ),
              icon_name = "cloud"
            )
          ),

          # ======================================================================
          # FORECAST TAB (Placeholder)
          # ======================================================================
          tabItem(
            tabName = "forecast",
            coming_soon_panel(
              title = "Weather Forecast",
              description = paste(
                "Weather forecast integration is coming soon.",
                "This will include multi-day forecasts, hourly predictions,",
                "and severe weather alerts."
              ),
              icon_name = "calendar-alt"
            )
          ),

          # ======================================================================
          # UNIFIED VIEW TAB (Placeholder)
          # ======================================================================
          tabItem(
            tabName = "unified",
            coming_soon_panel(
              title = "Unified Data View",
              description = paste(
                "The unified view will overlay data from your weather station,",
                "NOAA observations, and forecast data for easy comparison",
                "and validation of readings."
              ),
              icon_name = "layer-group"
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
  "
}
