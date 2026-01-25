# ==============================================================================
# config.R
# Configuration settings for Weather Station Dashboard
# ==============================================================================

# ------------------------------------------------------------------------------
# Database Configuration
# ------------------------------------------------------------------------------
# Note: For production, consider using environment variables for sensitive data
# e.g., Sys.getenv("DB_PASSWORD")

db_config <- list(
  host = "192.168.50.134",
  port = 5432,
  dbname = "weatherdata",
  user = "dlawton",
  password = Sys.getenv("DB_PASSWORD", "YOUR_PASSWORD")
)

# ------------------------------------------------------------------------------
# Timezone Configuration
# ------------------------------------------------------------------------------
timezone_db <- "UTC"
timezone_display <- "America/New_York"

# ------------------------------------------------------------------------------
# Station Configuration
# ------------------------------------------------------------------------------
default_station_id <- "ST-00175439"

# ------------------------------------------------------------------------------
# Refresh Intervals (in milliseconds)
# ------------------------------------------------------------------------------
refresh_intervals <- list(
  current_conditions = 60000, # 1 minute
  historical_data = 21600000 # 6 hours
)

# ------------------------------------------------------------------------------
# Alert Thresholds (defaults)
# ------------------------------------------------------------------------------
alert_thresholds <- list(
  # Precipitation thresholds (mm per observation period)
  heavy_rain_mm = 2.5,
  extreme_rain_mm = 7.5,

  # Wind thresholds (m/s)
  high_wind_ms = 10.0,
  extreme_wind_ms = 20.0,

  # Temperature thresholds (°C)
  freezing_temp_c = 0,
  heat_warning_c = 35,
  extreme_cold_c = -20,
  extreme_heat_c = 40,

  # Humidity thresholds (%)
  low_humidity = 20,
  high_humidity = 90,

  # Lightning detection (any count > 0 triggers alert)
  lightning_detected = 0,

  # Battery voltage threshold (low battery warning)
  low_battery_v = 2.4
)

# ------------------------------------------------------------------------------
# Time Window Options
# ------------------------------------------------------------------------------
time_windows <- list(
  "24h" = list(label = "Last 24 Hours", hours = 24),
  "7d" = list(label = "Last 7 Days", hours = 168),
  "30d" = list(label = "Last 30 Days", hours = 720),
  "3mo" = list(label = "Last 3 Months", hours = 2160),
  "6mo" = list(label = "Last 6 Months", hours = 4320),
  "1y" = list(label = "Last Year", hours = 8760),
  "all" = list(label = "All Time", hours = NULL)
)

# ------------------------------------------------------------------------------
# Color Palette - Modern Weather App Aesthetic
# ------------------------------------------------------------------------------
# A harmonious, accessible palette inspired by weather conditions
colors <- list(
  # Primary metric colors
  temperature = "#E63946", # Warm coral red
  humidity = "#457B9D", # Steel blue
  rainfall = "#1D3557", # Deep navy
  wind = "#2A9D8F", # Teal
  lightning = "#F4A261", # Warm amber
  pressure = "#6C757D", # Neutral gray


  # Alert colors
  alert_danger = "#DC3545",
  alert_warning = "#FFC107",
  alert_info = "#17A2B8",
  alert_success = "#28A745",


  # UI colors
  background = "#F8F9FA",
  card_bg = "#FFFFFF",
  text_primary = "#212529",
  text_secondary = "#6C757D",
  border = "#DEE2E6",


  # Gradient palette for charts (viridis-inspired but warmer)
  gradient = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")
)

# Plotly-compatible color scale
plotly_colors <- list(
  temperature = "rgb(230, 57, 70)",
  humidity = "rgb(69, 123, 157)",
  rainfall = "rgb(29, 53, 87)",
  wind_speed = "rgb(42, 157, 143)",
  wind_gust = "rgb(38, 70, 83)",
  lightning = "rgb(244, 162, 97)",
  pressure = "rgb(108, 117, 125)"
)

# ggplot2 theme colors
ggplot_theme <- list(
  background = "#FAFBFC",
  panel_bg = "#FFFFFF",
  grid_major = "#E9ECEF",
  grid_minor = "#F1F3F4",
  text = "#343A40",
  axis = "#495057"
)

# ------------------------------------------------------------------------------
# Unit Conversion Factors
# ------------------------------------------------------------------------------
conversions <- list(
  ms_to_kph = 3.6,
  ms_to_mph = 2.237,
  c_to_f = function(c) c * 9 / 5 + 32,
  f_to_c = function(f) (f - 32) * 5 / 9,
  mm_to_in = 0.0393701,
  in_to_mm = 25.4
)

# ------------------------------------------------------------------------------
# Data Aggregation Settings
# ------------------------------------------------------------------------------
aggregation <- list(
  # For displays longer than these hours, aggregate data
  aggregate_threshold_hours = 48,
  # Aggregation interval in minutes for long-range displays
  aggregate_interval_minutes = 15,
  # Thresholds for different aggregation levels
  thresholds = list(
    list(hours = 168, interval = "15 min"), # > 7 days: 15 min aggregation
    list(hours = 720, interval = "1 hour"), # > 30 days: 1 hour aggregation
    list(hours = 2160, interval = "6 hours"), # > 3 months: 6 hour aggregation
    list(hours = 4320, interval = "12 hours"), # > 6 months: 12 hour aggregation
    list(hours = 8760, interval = "1 day") # > 1 year: 1 day aggregation
  )
)

# ------------------------------------------------------------------------------
# Logging Configuration
# ------------------------------------------------------------------------------
logging <- list(
  enabled = TRUE,
  level = "INFO" # DEBUG, INFO, WARN, ERROR
)
