# ==============================================================================
# app.R
# Main application entry point for Weather Station Dashboard
# ==============================================================================

# ==============================================================================
# LOAD REQUIRED PACKAGES
# ==============================================================================

# Core Shiny packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)

# Data manipulation
library(dplyr)
library(tidyr)
library(lubridate)

# Database
library(pool)
library(DBI)
library(RPostgres)

# Visualization
library(ggplot2)
library(plotly)
library(scales)

# ==============================================================================
# SOURCE CONFIGURATION AND MODULES
# ==============================================================================

# Load configuration
source("config.R")

# Load modules in dependency order
source("R/db_access.R")
source("R/data_processing.R")
source("R/plot_weather.R")
source("R/ui_components.R")
source("R/ui_main.R")
source("R/server_logic.R")

# ==============================================================================
# DATABASE CONNECTION
# ==============================================================================

# Create database connection pool
# This is created once when the app starts and shared across all sessions
db_pool <- create_db_pool(db_config)

# Ensure pool is closed when app stops
onStop(function() {
  if (!is.null(db_pool)) {
    message("Closing database connection pool...")
    close_db_pool(db_pool)
  }
})

# ==============================================================================
# BUILD APPLICATION
# ==============================================================================

# Build UI
ui <- build_dashboard_ui()

# Build server with database pool
server <- build_server(db_pool)

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

# Create Shiny app object
shinyApp(ui = ui, server = server)
