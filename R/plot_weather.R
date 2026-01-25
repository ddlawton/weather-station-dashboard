# ==============================================================================
# plot_weather.R
# Visualization functions for Weather Station Dashboard
# Contains both Plotly (interactive) and ggplot2 (publication-quality) charts
# ==============================================================================

library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(data.table)

# Source configuration for colors
# Note: In production, config is loaded by app.R before this file

# ==============================================================================
# PLOTLY INTERACTIVE CHARTS
# ==============================================================================

# ------------------------------------------------------------------------------
#' Create Temperature Plot (Plotly)
#'
#' Creates an interactive temperature line chart with feels-like overlay.
#'
#' @param df Data frame with timestamp, air_temp, and optionally feels_like
#' @param show_feels_like Include feels-like temperature line (default: TRUE)
#' @param unit Temperature unit: "C" or "F" (default: "C")
#' @param height Plot height in pixels
#'
#' @return Plotly object
#' @export
# ------------------------------------------------------------------------------
plot_temperature_plotly <- function(df,
                                    show_feels_like = TRUE,
                                    unit = "C",
                                    height = 300) {
  if (nrow(df) == 0) {
    return(plotly_empty_message("No temperature data available"))
  }

  # Convert units if needed (vectorized, only once)
  if (unit == "F") {
    df$air_temp <- df$air_temp * 9 / 5 + 32
    if ("feels_like" %in% names(df)) {
      df$feels_like <- df$feels_like * 9 / 5 + 32
    }
  }

  unit_symbol <- if (unit == "F") "°F" else "°C"

  p <- plot_ly(df, x = ~timestamp, height = height) |>
    add_lines(
      y = ~air_temp,
      name = "Temperature",
      line = list(color = "rgb(230, 57, 70)", width = 2),
      hovertemplate = paste0(
        "<b>Temperature</b>: %{y:.1f}", unit_symbol, "<br>",
        "<b>Time</b>: %{x}<extra></extra>"
      )
    )

  if (show_feels_like && "feels_like" %in% names(df)) {
    p <- p |>
      add_lines(
        y = ~feels_like,
        name = "Feels Like",
        line = list(color = "rgb(230, 57, 70)", width = 1.5, dash = "dash"),
        hovertemplate = paste0(
          "<b>Feels Like</b>: %{y:.1f}", unit_symbol, "<br>",
          "<b>Time</b>: %{x}<extra></extra>"
        )
      )
  }

  p |>
    layout(
      title = list(text = "Temperature", x = 0),
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      yaxis = list(
        title = unit_symbol,
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.15),
      margin = list(t = 40, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ------------------------------------------------------------------------------
#' Create Humidity Plot (Plotly)
#'
#' Creates an interactive humidity chart with dew point.
#'
#' @param df Data frame with timestamp, humidity, and optionally dew_point
#' @param height Plot height in pixels
#'
#' @return Plotly object
#' @export
# ------------------------------------------------------------------------------
plot_humidity_plotly <- function(df, height = 300) {
  if (nrow(df) == 0) {
    return(plotly_empty_message("No humidity data available"))
  }

  p <- plot_ly(df, x = ~timestamp, height = height) |>
    add_lines(
      y = ~humidity,
      name = "Humidity",
      line = list(color = "rgb(69, 123, 157)", width = 2),
      hovertemplate = paste(
        "<b>Humidity</b>: %{y:.1f}%<br>",
        "<b>Time</b>: %{x}<extra></extra>"
      )
    )

  # Add dew point on secondary y-axis if available
  if ("dew_point" %in% names(df)) {
    p <- p |>
      add_lines(
        y = ~dew_point,
        name = "Dew Point",
        yaxis = "y2",
        line = list(color = "rgb(69, 123, 157)", width = 1.5, dash = "dot"),
        hovertemplate = paste(
          "<b>Dew Point</b>: %{y:.1f}°C<br>",
          "<b>Time</b>: %{x}<extra></extra>"
        )
      )
  }

  p |>
    layout(
      title = list(text = "Humidity", x = 0),
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      yaxis = list(
        title = "%",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)",
        range = c(0, 100)
      ),
      yaxis2 = list(
        title = "Dew Point (°C)",
        overlaying = "y",
        side = "right",
        showgrid = FALSE
      ),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.15),
      margin = list(t = 40, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ------------------------------------------------------------------------------
#' Create Wind Plot (Plotly)
#'
#' Creates an interactive wind speed chart with gust and lull bands.
#'
#' @param df Data frame with timestamp, wind_avg, wind_gust, wind_lull
#' @param unit Display unit: "ms", "kph", or "mph"
#' @param height Plot height in pixels
#'
#' @return Plotly object
#' @export
# ------------------------------------------------------------------------------
plot_wind_plotly <- function(df, unit = "kph", height = 300) {
  if (nrow(df) == 0) {
    return(plotly_empty_message("No wind data available"))
  }

  # Convert units (vectorized, single operation per column)
  multiplier <- switch(unit,
    "kph" = 3.6,
    "mph" = 2.237,
    1
  )
  unit_label <- switch(unit,
    "kph" = "km/h",
    "mph" = "mph",
    "m/s"
  )

  df$wind_avg_display <- df$wind_avg * multiplier
  df$wind_gust_display <- df$wind_gust * multiplier
  df$wind_lull_display <- df$wind_lull * multiplier

  plot_ly(df, x = ~timestamp, height = height) |>
    # Gust line (top)
    add_lines(
      y = ~wind_gust_display,
      name = "Gust",
      line = list(color = "rgba(38, 70, 83, 0.6)", width = 1),
      hovertemplate = paste0("<b>Gust</b>: %{y:.1f} ", unit_label, "<extra></extra>")
    ) |>
    # Average wind (main line)
    add_lines(
      y = ~wind_avg_display,
      name = "Average",
      line = list(color = "rgb(42, 157, 143)", width = 2.5),
      fill = "tonexty",
      fillcolor = "rgba(42, 157, 143, 0.15)",
      hovertemplate = paste0("<b>Average</b>: %{y:.1f} ", unit_label, "<extra></extra>")
    ) |>
    # Lull line (bottom)
    add_lines(
      y = ~wind_lull_display,
      name = "Lull",
      line = list(color = "rgba(42, 157, 143, 0.4)", width = 1),
      hovertemplate = paste0("<b>Lull</b>: %{y:.1f} ", unit_label, "<extra></extra>")
    ) |>
    layout(
      title = list(text = "Wind Speed", x = 0),
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      yaxis = list(
        title = unit_label,
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)",
        rangemode = "tozero"
      ),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.15),
      margin = list(t = 40, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ------------------------------------------------------------------------------
#' Create Wind Rose (Plotly)
#'
#' Creates a polar wind rose showing direction distribution.
#' Optimized binning logic using data.table for performance.
#'
#' @param df Data frame with wind_dir and wind_avg
#' @param n_bins Number of direction bins (default: 16)
#' @param height Plot height in pixels
#'
#' @return Plotly object
#' @export
# ------------------------------------------------------------------------------
plot_wind_rose_plotly <- function(df, n_bins = 16, height = 350) {
  if (nrow(df) == 0 || all(is.na(df$wind_dir))) {
    return(plotly_empty_message("No wind direction data available"))
  }

  # Create direction bins - optimized version
  bin_width <- 360 / n_bins

  # Filter and prepare data using data.table for speed
  dt <- as.data.table(df)
  dt <- dt[!is.na(wind_dir) & !is.na(wind_avg)]

  if (nrow(dt) == 0) {
    return(plotly_empty_message("No valid wind direction data available"))
  }

  # Vectorized binning and categorization
  dt[, `:=`(
    dir_bin = as.integer(floor((wind_dir + bin_width / 2) / bin_width)) %% n_bins,
    dir_center = (as.integer(floor((wind_dir + bin_width / 2) / bin_width)) %% n_bins) * bin_width,
    wind_avg_kph = wind_avg * 3.6
  )]

  dt[, speed_category := cut(
    wind_avg_kph,
    breaks = c(0, 5, 10, 20, 30, Inf),
    labels = c("< 5", "5-10", "10-20", "20-30", "> 30"),
    include.lowest = TRUE
  )]

  # Aggregate
  df_binned <- dt[, .(count = .N), by = .(dir_center, speed_category)]

  # Convert back to data frame for plotly
  df_binned <- as.data.frame(df_binned)

  # Color palette for speed categories
  speed_colors <- c(
    "< 5" = "rgba(42, 157, 143, 0.4)",
    "5-10" = "rgba(42, 157, 143, 0.6)",
    "10-20" = "rgba(42, 157, 143, 0.8)",
    "20-30" = "rgba(38, 70, 83, 0.8)",
    "> 30" = "rgba(230, 57, 70, 0.9)"
  )

  plot_ly(type = "barpolar", height = height) |>
    add_trace(
      data = df_binned,
      r = ~count,
      theta = ~dir_center,
      color = ~speed_category,
      colors = speed_colors,
      hovertemplate = paste(
        "<b>Direction</b>: %{theta}°<br>",
        "<b>Count</b>: %{r}<br>",
        "<extra></extra>"
      )
    ) |>
    layout(
      title = list(text = "Wind Rose", x = 0.5),
      polar = list(
        angularaxis = list(
          direction = "clockwise",
          rotation = 90,
          tickmode = "array",
          tickvals = seq(0, 315, 45),
          ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        radialaxis = list(
          showticklabels = TRUE,
          ticksuffix = ""
        )
      ),
      legend = list(title = list(text = "Speed (km/h)")),
      height = height,
      paper_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ------------------------------------------------------------------------------
#' Create Precipitation Plot (Plotly)
#'
#' Creates an interactive precipitation bar chart.
#'
#' @param df Data frame with timestamp and precip
#' @param unit Precipitation unit: "mm" or "in" (default: "mm")
#' @param height Plot height in pixels
#'
#' @return Plotly object
#' @export
# ------------------------------------------------------------------------------
plot_precipitation_plotly <- function(df, unit = "mm", height = 250) {
  if (nrow(df) == 0) {
    return(plotly_empty_message("No precipitation data available"))
  }

  # Convert units if needed (vectorized)
  if (unit == "in") {
    df$precip <- df$precip * 0.0393701
  }

  unit_label <- unit
  precision <- if (unit == "in") 4 else 3

  plot_ly(df, x = ~timestamp, height = height) |>
    add_bars(
      y = ~precip,
      marker = list(
        color = "rgb(29, 53, 87)",
        line = list(color = "rgb(29, 53, 87)", width = 0.5)
      ),
      hovertemplate = paste0(
        "<b>Precipitation</b>: %{y:.", precision, "f} ", unit_label, "<br>",
        "<b>Time</b>: %{x}<extra></extra>"
      )
    ) |>
    layout(
      title = list(text = "Precipitation", x = 0),
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      yaxis = list(
        title = unit_label,
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)",
        rangemode = "tozero"
      ),
      bargap = 0.1,
      margin = list(t = 40, b = 60),
      height = height,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ------------------------------------------------------------------------------
#' Create Pressure Plot (Plotly)
#'
#' Creates an interactive barometric pressure chart.
#'
#' @param df Data frame with timestamp and pressure
#' @param height Plot height in pixels
#'
#' @return Plotly object
#' @export
# ------------------------------------------------------------------------------
plot_pressure_plotly <- function(df, height = 250, y_limits = c(950, 1070)) {
  if (nrow(df) == 0) {
    return(plotly_empty_message("No pressure data available"))
  }

  layout_yaxis <- list(
    title = "hPa",
    showgrid = TRUE,
    gridcolor = "rgba(233, 236, 239, 0.8)"
  )

  if (!is.null(y_limits) && length(y_limits) == 2) {
    layout_yaxis$range <- y_limits
  }

  plot_ly(df, x = ~timestamp, height = height) |>
    add_lines(
      y = ~pressure,
      line = list(color = "rgb(108, 117, 125)", width = 2),
      fill = "tozeroy",
      fillcolor = "rgba(108, 117, 125, 0.1)",
      hovertemplate = paste(
        "<b>Pressure</b>: %{y:.1f} hPa<br>",
        "<b>Time</b>: %{x}<extra></extra>"
      )
    ) |>
    layout(
      title = list(text = "Barometric Pressure", x = 0),
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      yaxis = layout_yaxis,
      margin = list(t = 40, b = 60),
      height = height,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ------------------------------------------------------------------------------
#' Create Lightning Activity Plot (Plotly)
#'
#' Creates a scatter/bar chart showing lightning strikes.
#'
#' @param df Data frame with timestamp and lightning_count
#' @param height Plot height in pixels
#'
#' @return Plotly object
#' @export
# ------------------------------------------------------------------------------
plot_lightning_plotly <- function(df, height = 200, n_ticks = 5) {
  if (nrow(df) == 0 || sum(df$lightning_count, na.rm = TRUE) == 0) {
    return(plotly_empty_message("No lightning activity detected"))
  }

  # Filter to only times with lightning (vectorized)
  df_lightning <- df[df$lightning_count > 0 & !is.na(df$lightning_count), ]

  if (nrow(df_lightning) == 0) {
    return(plotly_empty_message("No lightning activity detected"))
  }

  # Calculate pretty y-axis ticks
  yvals <- df_lightning$lightning_count
  pretty_ticks <- pretty(yvals, n = n_ticks)

  plot_ly(df_lightning, x = ~timestamp, height = height) |>
    add_bars(
      y = ~lightning_count,
      marker = list(
        color = "rgb(244, 162, 97)",
        line = list(color = "rgb(244, 162, 97)", width = 0.5)
      ),
      hovertemplate = paste(
        "<b>Lightning Strikes</b>: %{y}<br>",
        "<b>Distance</b>: ", df_lightning$lightning_dist, " km<br>",
        "<b>Time</b>: %{x}<extra></extra>"
      )
    ) |>
    layout(
      title = list(text = "Lightning Activity", x = 0),
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      yaxis = list(
        title = "Strikes",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)",
        tickvals = pretty_ticks
      ),
      margin = list(t = 40, b = 60),
      height = height,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ------------------------------------------------------------------------------
#' Create Multi-Variable Overview Plot (Plotly)
#'
#' Creates a combined chart with temperature, humidity, and pressure.
#'
#' @param df Data frame with all weather variables
#' @param height Plot height in pixels
#'
#' @return Plotly object
#' @export
# ------------------------------------------------------------------------------
plot_overview_plotly <- function(df, height = 400) {
  if (nrow(df) == 0) {
    return(plotly_empty_message("No data available"))
  }

  plot_ly(df, x = ~timestamp, height = height) |>
    # Temperature
    add_lines(
      y = ~air_temp,
      name = "Temperature (°C)",
      line = list(color = "rgb(230, 57, 70)", width = 2),
      hovertemplate = "<b>Temp</b>: %{y:.1f}°C<extra></extra>"
    ) |>
    # Humidity (secondary axis)
    add_lines(
      y = ~humidity,
      name = "Humidity (%)",
      yaxis = "y2",
      line = list(color = "rgb(69, 123, 157)", width = 2),
      hovertemplate = "<b>Humidity</b>: %{y:.1f}%<extra></extra>"
    ) |>
    layout(
      title = list(text = "Weather Overview", x = 0),
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      yaxis = list(
        title = "Temperature (°C)",
        titlefont = list(color = "rgb(230, 57, 70)"),
        tickfont = list(color = "rgb(230, 57, 70)"),
        showgrid = TRUE,
        gridcolor = "rgba(233, 236, 239, 0.8)"
      ),
      yaxis2 = list(
        title = "Humidity (%)",
        titlefont = list(color = "rgb(69, 123, 157)"),
        tickfont = list(color = "rgb(69, 123, 157)"),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        range = c(0, 100)
      ),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.12),
      margin = list(t = 40, b = 80, r = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ==============================================================================
# GGPLOT2 STATIC CHARTS (Publication Quality)
# ==============================================================================

# ------------------------------------------------------------------------------
#' Weather Dashboard Theme for ggplot2
#'
#' A clean, modern theme for weather visualizations.
#'
#' @param base_size Base font size
#' @param base_family Base font family
#'
#' @return ggplot2 theme object
#' @export
# ------------------------------------------------------------------------------
theme_weather <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Background
      plot.background = element_rect(fill = "#FAFBFC", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),

      # Grid
      panel.grid.major = element_line(color = "#E9ECEF", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#F1F3F4", linewidth = 0.25),

      # Axes
      axis.line = element_line(color = "#495057", linewidth = 0.5),
      axis.ticks = element_line(color = "#495057"),
      axis.text = element_text(color = "#343A40"),
      axis.title = element_text(color = "#343A40", face = "bold"),

      # Title
      plot.title = element_text(
        color = "#212529",
        face = "bold",
        size = rel(1.2),
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        color = "#6C757D",
        size = rel(0.9),
        hjust = 0,
        margin = margin(b = 15)
      ),

      # Legend
      legend.background = element_rect(fill = "#FFFFFF", color = "#DEE2E6"),
      legend.key = element_rect(fill = "#FFFFFF", color = NA),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",

      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ------------------------------------------------------------------------------
#' Create Temperature Plot (ggplot2)
#'
#' Creates a publication-quality temperature chart.
#'
#' @param df Data frame with timestamp and air_temp
#' @param title Optional custom title
#'
#' @return ggplot2 object
#' @export
# ------------------------------------------------------------------------------
plot_temperature_ggplot <- function(df, title = "Temperature") {
  if (nrow(df) == 0) {
    return(ggplot_empty_message("No temperature data available"))
  }

  p <- ggplot(df, aes(x = timestamp, y = air_temp)) +
    geom_line(color = "#E63946", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#6C757D", alpha = 0.7) +
    scale_y_continuous(labels = function(x) paste0(x, "°C")) +
    scale_x_datetime(labels = date_format("%b %d\n%H:%M", tz = "America/New_York")) +
    labs(
      title = title,
      x = NULL,
      y = "Temperature"
    ) +
    theme_weather()

  # Add feels-like if available
  if ("feels_like" %in% names(df)) {
    p <- p +
      geom_line(aes(y = feels_like),
        color = "#E63946", linewidth = 0.7,
        linetype = "dashed", alpha = 0.7
      )
  }

  return(p)
}

# ------------------------------------------------------------------------------
#' Create Humidity Plot (ggplot2)
#'
#' Creates a publication-quality humidity chart.
#'
#' @param df Data frame with timestamp and humidity
#' @param title Optional custom title
#'
#' @return ggplot2 object
#' @export
# ------------------------------------------------------------------------------
plot_humidity_ggplot <- function(df, title = "Relative Humidity") {
  if (nrow(df) == 0) {
    return(ggplot_empty_message("No humidity data available"))
  }

  ggplot(df, aes(x = timestamp, y = humidity)) +
    geom_area(fill = "#457B9D", alpha = 0.3) +
    geom_line(color = "#457B9D", linewidth = 1) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    scale_x_datetime(labels = date_format("%b %d\n%H:%M", tz = "America/New_York")) +
    labs(
      title = title,
      x = NULL,
      y = "Humidity"
    ) +
    theme_weather()
}

# ------------------------------------------------------------------------------
#' Create Wind Plot (ggplot2)
#'
#' Creates a publication-quality wind speed chart with ribbon for gust/lull.
#'
#' @param df Data frame with timestamp, wind_avg, wind_gust, wind_lull
#' @param unit Display unit: "ms", "kph", or "mph"
#' @param title Optional custom title
#'
#' @return ggplot2 object
#' @export
# ------------------------------------------------------------------------------
plot_wind_ggplot <- function(df, unit = "kph", title = "Wind Speed") {
  if (nrow(df) == 0) {
    return(ggplot_empty_message("No wind data available"))
  }

  # Convert units (vectorized)
  multiplier <- switch(unit,
    "kph" = 3.6,
    "mph" = 2.237,
    1
  )
  unit_label <- switch(unit,
    "kph" = "km/h",
    "mph" = "mph",
    "m/s"
  )

  df$wind_avg_display <- df$wind_avg * multiplier
  df$wind_gust_display <- df$wind_gust * multiplier
  df$wind_lull_display <- df$wind_lull * multiplier

  ggplot(df, aes(x = timestamp)) +
    geom_ribbon(
      aes(ymin = wind_lull_display, ymax = wind_gust_display),
      fill = "#2A9D8F",
      alpha = 0.2
    ) +
    geom_line(aes(y = wind_avg_display), color = "#2A9D8F", linewidth = 1.2) +
    geom_line(aes(y = wind_gust_display),
      color = "#264653", linewidth = 0.5,
      linetype = "dashed", alpha = 0.7
    ) +
    scale_y_continuous(labels = function(x) paste0(x, " ", unit_label)) +
    scale_x_datetime(labels = date_format("%b %d\n%H:%M", tz = "America/New_York")) +
    labs(
      title = title,
      x = NULL,
      y = paste("Speed (", unit_label, ")")
    ) +
    theme_weather()
}

# ------------------------------------------------------------------------------
#' Create Precipitation Plot (ggplot2)
#'
#' Creates a publication-quality precipitation bar chart.
#'
#' @param df Data frame with timestamp and precip
#' @param title Optional custom title
#'
#' @return ggplot2 object
#' @export
# ------------------------------------------------------------------------------
plot_precipitation_ggplot <- function(df, title = "Precipitation") {
  if (nrow(df) == 0) {
    return(ggplot_empty_message("No precipitation data available"))
  }

  ggplot(df, aes(x = timestamp, y = precip)) +
    geom_col(fill = "#1D3557", alpha = 0.8, width = 60) +
    scale_y_continuous(labels = function(x) paste0(x, " mm")) +
    scale_x_datetime(labels = date_format("%b %d\n%H:%M", tz = "America/New_York")) +
    labs(
      title = title,
      x = NULL,
      y = "Precipitation (mm)"
    ) +
    theme_weather()
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
#' Create Empty Plotly Message
#'
#' Creates a placeholder plotly chart with a message.
#'
#' @param message Message to display
#'
#' @return Plotly object
# ------------------------------------------------------------------------------
plotly_empty_message <- function(message = "No data available") {
  plot_ly() |>
    add_annotations(
      text = message,
      x = 0.5,
      y = 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(size = 14, color = "#6C757D")
    ) |>
    layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF"
    )
}

# ------------------------------------------------------------------------------
#' Create Empty ggplot Message
#'
#' Creates a placeholder ggplot chart with a message.
#'
#' @param message Message to display
#'
#' @return ggplot2 object
# ------------------------------------------------------------------------------
ggplot_empty_message <- function(message = "No data available") {
  ggplot() +
    annotate("text",
      x = 0.5, y = 0.5, label = message,
      size = 5, color = "#6C757D"
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#FAFBFC", color = NA)
    )
}
