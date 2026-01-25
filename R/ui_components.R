# ==============================================================================
# ui_components.R
# Reusable UI components for Weather Station Dashboard
# Includes value boxes, info panels, and custom widgets
# ==============================================================================

library(shiny)
library(shinydashboard)
library(htmltools)

# ==============================================================================
# VALUE BOX COMPONENTS
# ==============================================================================

# ------------------------------------------------------------------------------
#' Create Weather Value Box
#'
#' Creates a styled value box for displaying current weather metrics.
#'
#' @param value The value to display
#' @param subtitle Description/label for the value
#' @param icon Font Awesome icon name (without "fa-" prefix)
#' @param color Color name: "temperature", "humidity", "wind", "rain",
#'   "lightning", "pressure", or standard bootstrap colors
#' @param width Bootstrap column width (default: 3)
#' @param alert_status Optional alert status: "normal", "warning", "danger"
#'
#' @return A Shiny tag object
#' @export
# ------------------------------------------------------------------------------
weather_value_box <- function(value,
                              subtitle,
                              icon = "thermometer-half",
                              color = "temperature",
                              width = 3,
                              alert_status = "normal") {
  # Map custom color names to hex values
  color_map <- list(
    temperature = "#E63946",
    humidity = "#457B9D",
    wind = "#2A9D8F",
    rain = "#1D3557",
    lightning = "#F4A261",
    pressure = "#6C757D",
    normal = "#28A745",
    warning = "#FFC107",
    danger = "#DC3545",
    info = "#17A2B8"
  )

  # Get background color based on alert status or color parameter
  bg_color <- if (alert_status == "danger") {
    color_map$danger
  } else if (alert_status == "warning") {
    color_map$warning
  } else if (color %in% names(color_map)) {
    color_map[[color]]
  } else {
    color
    # Assume it's a hex color
  }

  # Determine text color (white for dark backgrounds, dark for light)
  text_color <- if (alert_status == "warning") "#212529" else "#FFFFFF"

  div(
    class = paste0("col-lg-", width, " col-md-6"),
    div(
      class = "weather-value-box",
      style = paste0(
        "background-color: ", bg_color, "; ",
        "color: ", text_color, "; ",
        "border-radius: 8px; ",
        "padding: 20px; ",
        "margin-bottom: 15px; ",
        "box-shadow: 0 2px 8px rgba(0,0,0,0.1);"
      ),
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        div(
          div(
            class = "value-box-value",
            style = "font-size: 2.5rem; font-weight: bold; line-height: 1.2;",
            value
          ),
          div(
            class = "value-box-subtitle",
            style = "font-size: 0.95rem; opacity: 0.9; margin-top: 5px;",
            subtitle
          )
        ),
        div(
          class = "value-box-icon",
          style = "font-size: 3rem; opacity: 0.7;",
          icon(icon)
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
#' Create Current Conditions Panel
#'
#' Creates a grid of value boxes for current weather conditions.
#'
#' @param observation Single-row data frame with current observation
#' @param alerts List of alert conditions from check_alert_conditions()
#' @param precip_24h 24-hour precipitation total
#' @param lightning_24h 24-hour lightning count
#'
#' @return A Shiny fluidRow object
#' @export
# ------------------------------------------------------------------------------
current_conditions_panel <- function(observation,
                                     alerts = NULL,
                                     precip_24h = 0,
                                     lightning_24h = 0,
                                     units_system = "metric") {
  if (nrow(observation) == 0) {
    return(
      div(
        class = "alert alert-info",
        icon("info-circle"),
        " No current observation data available. The weather station may be offline."
      )
    )
  }

  obs <- observation[1, ]

  # Determine alert statuses
  temp_alert <- "normal"
  if (!is.null(alerts) &&
    (isTRUE(alerts$conditions$extreme_cold$active) ||
      isTRUE(alerts$conditions$extreme_heat$active))) {
    temp_alert <- "danger"
  } else if (!is.null(alerts) &&
    (isTRUE(alerts$conditions$freezing$active) ||
      isTRUE(alerts$conditions$heat_warning$active))) {
    temp_alert <- "warning"
  }

  wind_alert <- "normal"
  if (!is.null(alerts) && isTRUE(alerts$conditions$extreme_wind$active)) {
    wind_alert <- "danger"
  } else if (!is.null(alerts) && isTRUE(alerts$conditions$high_wind$active)) {
    wind_alert <- "warning"
  }

  rain_alert <- "normal"
  if (!is.null(alerts) && isTRUE(alerts$conditions$extreme_rain$active)) {
    rain_alert <- "danger"
  } else if (!is.null(alerts) && isTRUE(alerts$conditions$heavy_rain$active)) {
    rain_alert <- "warning"
  }

  lightning_alert <- if (lightning_24h > 0) "danger" else "normal"

  # Format values based on units_system
  is_us <- identical(units_system, "us")
  # Temperature
  temp_value <- if (!is.na(obs$air_temp)) {
    if (is_us) {
      paste0(round(obs$air_temp * 9 / 5 + 32, 1), "°F")
    } else {
      paste0(round(obs$air_temp, 1), "°C")
    }
  } else {
    "--"
  }
  # Humidity
  humidity_value <- if (!is.na(obs$humidity)) {
    paste0(round(obs$humidity, 0), "%")
  } else {
    "--"
  }
  # Wind
  wind_value <- if (!is.na(obs$wind_avg)) {
    if (is_us) {
      paste0(round(obs$wind_avg * 2.237, 1), " mph")
    } else {
      paste0(round(obs$wind_avg, 1), " m/s")
    }
  } else {
    "--"
  }
  wind_dir <- if (!is.na(obs$wind_dir)) {
    paste0(" ", get_wind_cardinal(obs$wind_dir))
  } else {
    ""
  }
  # Precipitation
  precip_value <- if (is_us) {
    paste0(round(precip_24h * 0.0393701, 2), " in")
  } else {
    paste0(round(precip_24h, 2), " mm")
  }
  # Pressure (always hPa)
  pressure_value <- if (!is.na(obs$pressure)) {
    paste0(round(obs$pressure, 1), " hPa")
  } else {
    "--"
  }
  lightning_value <- as.character(lightning_24h)

  fluidRow(
    weather_value_box(
      value = temp_value,
      subtitle = "Temperature",
      icon = "thermometer-half",
      color = "temperature",
      width = 2,
      alert_status = temp_alert
    ),
    weather_value_box(
      value = humidity_value,
      subtitle = "Humidity",
      icon = "tint",
      color = "humidity",
      width = 2,
      alert_status = "normal"
    ),
    weather_value_box(
      value = wind_value,
      subtitle = paste0("Wind", wind_dir),
      icon = "wind",
      color = "wind",
      width = 2,
      alert_status = wind_alert
    ),
    weather_value_box(
      value = precip_value,
      subtitle = "Rain (24h)",
      icon = "cloud-rain",
      color = "rain",
      width = 2,
      alert_status = rain_alert
    ),
    weather_value_box(
      value = pressure_value,
      subtitle = "Pressure",
      icon = "tachometer-alt",
      color = "pressure",
      width = 2,
      alert_status = "normal"
    ),
    weather_value_box(
      value = lightning_value,
      subtitle = "Lightning (24h)",
      icon = "bolt",
      color = "lightning",
      width = 2,
      alert_status = lightning_alert
    )
  )
}

# Helper function for wind cardinal direction (vectorized version)
# NOTE: This must match the vectorized implementation in data_processing.R
get_wind_cardinal <- function(degrees) {
  degrees <- as.numeric(degrees)
  n <- length(degrees)
  if (n == 0) {
    return(character(0))
  }
  result <- rep(NA_character_, n)
  cardinals <- c(
    "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"
  )
  valid <- !is.na(degrees) & is.finite(degrees)
  if (any(valid)) {
    deg <- degrees[valid] %% 360
    idx <- ((round(deg / 22.5) %% 16) + 1)
    result[valid] <- cardinals[idx]
  }
  return(result)
}

# ==============================================================================
# ALERT COMPONENTS
# ==============================================================================

# ------------------------------------------------------------------------------
#' Create Alert Banner
#'
#' Creates an alert banner when weather conditions exceed thresholds.
#'
#' @param alerts List of alert conditions from check_alert_conditions()
#'
#' @return A Shiny tag object or NULL if no alerts
#' @export
# ------------------------------------------------------------------------------
alert_banner <- function(alerts) {
  if (is.null(alerts) || !alerts$has_alerts) {
    return(NULL)
  }

  # Collect all alert messages
  alert_messages <- lapply(alerts$conditions, function(cond) {
    if (isTRUE(cond$active)) {
      div(
        class = paste0("alert-item alert-", cond$severity),
        icon(if (cond$severity == "danger") "exclamation-triangle" else "exclamation-circle"),
        span(cond$message)
      )
    }
  })

  # Filter out NULLs
  alert_messages <- Filter(Negate(is.null), alert_messages)

  if (length(alert_messages) == 0) {
    return(NULL)
  }

  # Determine overall severity
  has_danger <- any(sapply(alerts$conditions, function(x) x$severity == "danger"))
  banner_class <- if (has_danger) "alert-danger" else "alert-warning"

  div(
    class = paste("alert", banner_class),
    style = "margin-bottom: 20px;",
    div(
      class = "alert-content",
      style = "display: flex; flex-wrap: wrap; gap: 15px;",
      alert_messages
    )
  )
}

# ==============================================================================
# CONTROL COMPONENTS
# ==============================================================================

# ------------------------------------------------------------------------------
#' Create Dashboard Controls Panel
#'
#' Creates the control panel with time window, refresh, and threshold settings.
#'
#' @param time_windows List of time window options
#' @param default_window Default selected time window
#'
#' @return A Shiny wellPanel object
#' @export
# ------------------------------------------------------------------------------
dashboard_controls <- function(time_windows, default_window = "24h") {
  div(
    class = "dashboard-controls",
    style = "padding: 15px; background-color: #2c3e50; border-radius: 4px; margin-bottom: 15px;",

    # Time Window
    selectInput(
      inputId = "time_window",
      label = tags$span(icon("clock"), " Time Window"),
      choices = setNames(
        names(time_windows),
        sapply(time_windows, function(x) x$label)
      ),
      selected = default_window,
      width = "100%"
    ),

    # Auto Refresh
    selectInput(
      inputId = "refresh_rate",
      label = tags$span(icon("sync"), " Auto Refresh"),
      choices = c(
        "Off" = 0,
        "Every 1 min" = 60000,
        "Every 5 min" = 300000,
        "Every 15 min" = 900000
      ),
      selected = 60000,
      width = "100%"
    ),

    # Units Toggle
    radioButtons(
      inputId = "units_system",
      label = tags$span(icon("balance-scale"), " Units"),
      choices = c("Metric" = "metric", "US" = "us"),
      selected = "metric",
      inline = TRUE,
      width = "100%"
    ),

    # Refresh Button
    actionButton(
      inputId = "refresh_now",
      label = "Refresh Now",
      icon = icon("sync"),
      width = "100%",
      class = "btn-primary",
      style = "margin-top: 10px;"
    )
  )
}

# ==============================================================================
# PLACEHOLDER COMPONENTS
# ==============================================================================

# ------------------------------------------------------------------------------
#' Create Coming Soon Panel
#'
#' Creates a placeholder panel for features under development.
#'
#' @param title Feature title
#' @param description Brief description of the upcoming feature
#' @param icon_name Font Awesome icon name
#'
#' @return A Shiny div object
#' @export
# ------------------------------------------------------------------------------
coming_soon_panel <- function(title = "Coming Soon",
                              description = "This feature is under development.",
                              icon_name = "wrench") {
  div(
    class = "coming-soon-panel",
    style = paste(
      "text-align: center;",
      "padding: 80px 40px;",
      "background: linear-gradient(135deg, #F8F9FA 0%, #E9ECEF 100%);",
      "border-radius: 12px;",
      "border: 2px dashed #DEE2E6;",
      "margin: 20px;"
    ),
    div(
      style = "font-size: 4rem; color: #ADB5BD; margin-bottom: 20px;",
      icon(icon_name)
    ),
    h3(
      style = "color: #495057; margin-bottom: 15px;",
      title
    ),
    p(
      style = "color: #6C757D; font-size: 1.1rem; max-width: 500px; margin: 0 auto;",
      description
    )
  )
}

# ==============================================================================
# STATUS COMPONENTS
# ==============================================================================

# ------------------------------------------------------------------------------
#' Create Database Status Indicator
#'
#' Creates a status indicator showing database connection health.
#'
#' @param is_connected Logical indicating connection status
#' @param last_update POSIXct time of last successful data fetch
#'
#' @return A Shiny span object
#' @export
# ------------------------------------------------------------------------------
db_status_indicator <- function(is_connected = TRUE, last_update = NULL) {
  if (is_connected) {
    status_color <- "#28A745"
    status_text <- "Connected"
    status_icon <- "check-circle"
  } else {
    status_color <- "#DC3545"
    status_text <- "Disconnected"
    status_icon <- "times-circle"
  }

  update_text <- if (!is.null(last_update)) {
    paste("Last update:", format(last_update, "%H:%M:%S"))
  } else {
    ""
  }

  span(
    class = "db-status",
    style = "font-size: 0.85rem;",
    span(
      style = paste0("color: ", status_color, ";"),
      icon(status_icon),
      status_text
    ),
    if (nchar(update_text) > 0) {
      span(
        style = "color: #6C757D; margin-left: 15px;",
        update_text
      )
    }
  )
}

# ------------------------------------------------------------------------------
#' Create Station Info Panel
#'
#' Creates a panel showing station and device information.
#'
#' @param station_id Station identifier
#' @param device_status Device status data frame
#' @param hub_status Hub status data frame
#'
#' @return A Shiny wellPanel object
#' @export
# ------------------------------------------------------------------------------
station_info_panel <- function(station_id, device_status = NULL, hub_status = NULL) {
  # Get latest device info
  device_info <- if (!is.null(device_status) && nrow(device_status) > 0) {
    device_status[1, ]
  } else {
    NULL
  }

  hub_info <- if (!is.null(hub_status) && nrow(hub_status) > 0) {
    hub_status[1, ]
  } else {
    NULL
  }

  wellPanel(
    style = "background-color: #FFFFFF; border: 1px solid #DEE2E6; border-radius: 8px;",
    h5(icon("broadcast-tower"), " Station Information", style = "color: #495057;"),
    hr(style = "margin: 10px 0;"),
    fluidRow(
      column(
        width = 6,
        p(strong("Station ID: "), station_id),
        if (!is.null(device_info)) {
          tagList(
            p(strong("Battery: "), paste0(round(device_info$voltage, 2), " V")),
            p(strong("Signal: "), paste0(device_info$rssi, " dBm")),
            p(strong("Firmware: "), device_info$firmware_revision)
          )
        }
      ),
      column(
        width = 6,
        if (!is.null(hub_info)) {
          tagList(
            p(strong("Hub: "), hub_info$hub_sn),
            p(strong("Hub Uptime: "), format_uptime(hub_info$uptime)),
            p(strong("Hub RSSI: "), paste0(hub_info$rssi, " dBm"))
          )
        }
      )
    )
  )
}

# Helper function to format uptime
format_uptime <- function(seconds) {
  if (is.na(seconds)) {
    return("--")
  }

  days <- floor(seconds / 86400)
  hours <- floor((seconds %% 86400) / 3600)

  if (days > 0) {
    paste0(days, "d ", hours, "h")
  } else {
    paste0(hours, "h")
  }
}
