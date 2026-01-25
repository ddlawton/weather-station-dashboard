# ==============================================================================
# global.R
# Global settings and package loading for Weather Station Dashboard
# This file is automatically sourced by Shiny before app.R
# ==============================================================================

# ==============================================================================
# PACKAGE INSTALLATION CHECK
# ==============================================================================

#' Check and Install Required Packages
#'
#' Checks if required packages are installed and installs missing ones.
#'
#' @param packages Character vector of package names
check_packages <- function(packages) {
  missing <- packages[!packages %in% installed.packages()[, "Package"]]

  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

# Required packages
required_packages <- c(
  # Shiny
  "shiny",
  "shinydashboard",
  "shinycssloaders",

  # Data
  "dplyr",
  "tidyr",
  "lubridate",

  # Database
  "pool",
  "DBI",
  "RPostgres",

  # Visualization
  "ggplot2",
  "plotly",
  "scales",

  # Utilities
  "htmltools"
)

# Check packages (comment out in production if packages are pre-installed)
# check_packages(required_packages)

# ==============================================================================
# GLOBAL OPTIONS
# ==============================================================================

# Set default timezone for the R session
Sys.setenv(TZ = "UTC")

# Shiny options
options(
  # Increase max upload size if needed
  shiny.maxRequestSize = 50 * 1024^2,

  # Enable Shiny auto-reload for development
  # shiny.autoreload = TRUE,

  # Suppress scientific notation
  scipen = 999,

  # Default number of digits
  digits = 4
)

# ggplot2 default theme
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::theme_set(ggplot2::theme_minimal())
}

# ==============================================================================
# ENVIRONMENT VARIABLES
# ==============================================================================

# Check for database password in environment
if (Sys.getenv("DB_PASSWORD") == "") {
  message("Note: DB_PASSWORD environment variable not set. Using default from config.")
  message("Set with: Sys.setenv(DB_PASSWORD = 'your_password')")
}

# ==============================================================================
# LOGGING SETUP
# ==============================================================================

#' Simple logging function
#'
#' @param message Message to log
#' @param level Log level (DEBUG, INFO, WARN, ERROR)
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, level, message))
}

message("Weather Station Dashboard initialized")
message(paste("R version:", R.version.string))
message(paste("Working directory:", getwd()))
