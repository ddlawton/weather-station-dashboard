# ==============================================================================
# data_processing.R
# Data processing and transformation functions for Weather Station Dashboard
# Handles aggregation, unit conversion, and derived calculations
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

# ------------------------------------------------------------------------------
#' Convert Timestamp to Display Timezone
#'
#' Converts UTC timestamps to the display timezone (America/New_York).
#'
#' @param df Data frame containing a 'timestamp' column
#' @param timestamp_col Name of the timestamp column (default: "timestamp")
#' @param display_tz Target timezone (default: "America/New_York")
#'
#' @return Data frame with converted timestamp
#' @export
# ------------------------------------------------------------------------------
convert_to_display_tz <- function(df,
                                  timestamp_col = "timestamp",
                                  display_tz = "America/New_York") {
  if (nrow(df) == 0 || !timestamp_col %in% names(df)) {
    return(df)
  }
  df[[timestamp_col]] <- with_tz(df[[timestamp_col]], tzone = display_tz)
  return(df)
}

# ------------------------------------------------------------------------------
#' Convert Wind Speed Units
#'
#' Converts wind speed from m/s to other units.
#'
#' @param speed_ms Wind speed in meters per second
#' @param to Target unit: "kph", "mph", or "ms" (default: "kph")
#'
#' @return Numeric wind speed in target unit
#' @export
# ------------------------------------------------------------------------------
convert_wind_speed <- function(speed_ms, to = "kph") {
  if (is.null(speed_ms) || all(is.na(speed_ms))) {
    return(NA_real_)
  }

  switch(to,
    "kph" = speed_ms * 3.6,
    "mph" = speed_ms * 2.237,
    "ms" = speed_ms,
    speed_ms
    # Default: return as-is
  )
}

# ------------------------------------------------------------------------------
#' Convert Temperature Units
#'
#' Converts temperature between Celsius and Fahrenheit.
#'
#' @param temp Temperature value
#' @param from Source unit: "C" or "F"
#' @param to Target unit: "C" or "F"
#'
#' @return Numeric temperature in target unit
#' @export
# ------------------------------------------------------------------------------
convert_temperature <- function(temp, from = "C", to = "C") {
  if (is.null(temp) || all(is.na(temp))) {
    return(NA_real_)
  }

  if (from == to) {
    return(temp)
  }

  if (from == "C" && to == "F") {
    return(temp * 9 / 5 + 32)
  } else if (from == "F" && to == "C") {
    return((temp - 32) * 5 / 9)
  }

  return(temp)
}

# ------------------------------------------------------------------------------
#' Convert Precipitation Units
#'
#' Converts precipitation between mm and inches.
#'
#' @param precip Precipitation value
#' @param from Source unit: "mm" or "in"
#' @param to Target unit: "mm" or "in"
#'
#' @return Numeric precipitation in target unit
#' @export
# ------------------------------------------------------------------------------
convert_precipitation <- function(precip, from = "mm", to = "mm") {
  if (is.null(precip) || all(is.na(precip))) {
    return(NA_real_)
  }

  if (from == to) {
    return(precip)
  }

  if (from == "mm" && to == "in") {
    return(precip * 0.0393701)
  } else if (from == "in" && to == "mm") {
    return(precip * 25.4)
  }

  return(precip)
}

# ------------------------------------------------------------------------------
#' Get Wind Direction Cardinal
#'
#' Converts wind direction in degrees to cardinal direction.
#' Fully vectorized, handles all edge cases.
#'
#' @param degrees Wind direction in degrees (0-360), can be a vector
#'
#' @return Character cardinal direction (e.g., "N", "NE", "E", etc.)
#' @export
# ------------------------------------------------------------------------------
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

# Backward compatibility alias
get_wind_cardinal_vec <- get_wind_cardinal

# ------------------------------------------------------------------------------
#' Calculate Feels Like Temperature
#'
#' Calculates apparent temperature based on wind chill or heat index.
#' Fully vectorized, handles all edge cases.
#'
#' @param temp_c Temperature in Celsius (vector)
#' @param humidity Relative humidity (%) (vector)
#' @param wind_ms Wind speed in m/s (vector)
#'
#' @return Numeric feels-like temperature in Celsius (vector)
#' @export
# ------------------------------------------------------------------------------
calculate_feels_like <- function(temp_c, humidity, wind_ms) {
  # Ensure plain numeric vectors
  temp_c <- as.numeric(temp_c)
  humidity <- as.numeric(humidity)
  wind_ms <- as.numeric(wind_ms)

  n <- max(length(temp_c), length(humidity), length(wind_ms))

  # Handle empty input
  if (n == 0) {
    return(numeric(0))
  }

  # Recycle to same length if needed
  if (length(temp_c) < n) temp_c <- rep_len(temp_c, n)
  if (length(humidity) < n) humidity <- rep_len(humidity, n)
  if (length(wind_ms) < n) wind_ms <- rep_len(wind_ms, n)

  # Initialize with actual temperature
  result <- temp_c

  # Convert wind to kph
  wind_kph <- wind_ms * 3.6

  # Wind Chill (cold + wind)
  wc_valid <- !is.na(temp_c) & !is.na(wind_kph) &
    is.finite(temp_c) & is.finite(wind_kph) &
    temp_c <= 10 & wind_kph > 4.8

  if (any(wc_valid)) {
    tc <- temp_c[wc_valid]
    wk <- wind_kph[wc_valid]
    result[wc_valid] <- round(
      13.12 + 0.6215 * tc - 11.37 * (wk^0.16) + 0.3965 * tc * (wk^0.16),
      1
    )
  }

  # Heat Index (hot + humid)
  hi_valid <- !is.na(temp_c) & !is.na(humidity) &
    is.finite(temp_c) & is.finite(humidity) &
    temp_c >= 27 & humidity >= 40

  if (any(hi_valid)) {
    temp_f <- temp_c[hi_valid] * 9 / 5 + 32
    h <- humidity[hi_valid]

    hi_f <- -42.379 + 2.04901523 * temp_f + 10.14333127 * h -
      0.22475541 * temp_f * h - 0.00683783 * temp_f^2 -
      0.05481717 * h^2 + 0.00122874 * temp_f^2 * h +
      0.00085282 * temp_f * h^2 - 0.00000199 * temp_f^2 * h^2

    result[hi_valid] <- round((hi_f - 32) * 5 / 9, 1)
  }

  return(result)
}

# Backward compatibility alias
calculate_feels_like_vec <- calculate_feels_like

# ------------------------------------------------------------------------------
#' Calculate Dew Point
#'
#' Calculates dew point from temperature and humidity using Magnus formula.
#' Fully vectorized, handles all edge cases.
#'
#' @param temp_c Temperature in Celsius (vector)
#' @param humidity Relative humidity (%) (vector)
#'
#' @return Numeric dew point in Celsius (vector)
#' @export
# ------------------------------------------------------------------------------
calculate_dew_point <- function(temp_c, humidity) {
  # Ensure plain numeric vectors
  temp_c <- as.numeric(temp_c)
  humidity <- as.numeric(humidity)

  n <- max(length(temp_c), length(humidity))

  # Handle empty input
  if (n == 0) {
    return(numeric(0))
  }

  # Recycle to same length if needed
  if (length(temp_c) < n) temp_c <- rep_len(temp_c, n)
  if (length(humidity) < n) humidity <- rep_len(humidity, n)

  # Initialize with NAs
  result <- rep(NA_real_, n)

  # Find valid values
  valid <- !is.na(temp_c) & !is.na(humidity) &
    is.finite(temp_c) & is.finite(humidity) &
    humidity > 0

  if (any(valid)) {
    # Magnus formula constants
    a <- 17.27
    b <- 237.7

    tc <- temp_c[valid]
    h <- humidity[valid]

    alpha <- (a * tc / (b + tc)) + log(h / 100)
    result[valid] <- round((b * alpha) / (a - alpha), 1)
  }

  return(result)
}

# Backward compatibility alias
calculate_dew_point_vec <- calculate_dew_point

# ------------------------------------------------------------------------------
#' Aggregate Observations by Time Interval
#'
#' Aggregates weather observations into specified time intervals.
#' Optimized using data.table for better performance on large datasets.
#'
#' @param df Data frame of observations
#' @param interval Aggregation interval (e.g., "15 min", "1 hour", "1 day")
#' @param timestamp_col Name of timestamp column
#'
#' @return Aggregated data frame
#' @export
# ------------------------------------------------------------------------------
aggregate_observations <- function(df,
                                   interval = "15 min",
                                   timestamp_col = "timestamp") {
  if (nrow(df) == 0) {
    return(df)
  }

  # Convert to data.table for faster operations
  dt <- as.data.table(df)

  # Create time buckets
  dt[, time_bucket := floor_date(get(timestamp_col), unit = interval)]

  # Determine grouping variables - check if station_id exists
  has_station_id <- "station_id" %in% names(dt)
  by_vars <- if (has_station_id) c("time_bucket", "station_id") else "time_bucket"

  # Perform aggregation using data.table (much faster than dplyr)
  result <- dt[, .(
    air_temp = mean(air_temp, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    pressure = mean(pressure, na.rm = TRUE),
    wind_lull = min(wind_lull, na.rm = TRUE),
    wind_avg = mean(wind_avg, na.rm = TRUE),
    wind_gust = max(wind_gust, na.rm = TRUE),
    wind_dir = circular_mean(wind_dir),
    precip = sum(precip, na.rm = TRUE),
    lightning_count = sum(lightning_count, na.rm = TRUE),
    lightning_dist = mean(lightning_dist, na.rm = TRUE),
    solar_rad = mean(solar_rad, na.rm = TRUE),
    uv = mean(uv, na.rm = TRUE),
    illuminance = mean(illuminance, na.rm = TRUE),
    battery = mean(battery, na.rm = TRUE),
    precip_type = max(precip_type, na.rm = TRUE),
    observation_count = .N
  ), by = by_vars]

  # Rename and sort
  setnames(result, "time_bucket", timestamp_col)
  setorderv(result, timestamp_col)

  # Convert back to tibble for compatibility with existing code
  return(as_tibble(result))
}

# ------------------------------------------------------------------------------
#' Calculate Circular Mean for Wind Direction
#'
#' Properly averages wind direction using circular statistics.
#'
#' @param degrees Vector of wind directions in degrees
#'
#' @return Numeric mean direction in degrees (0-360)
#' @export
# ------------------------------------------------------------------------------
circular_mean <- function(degrees) {
  if (all(is.na(degrees))) {
    return(NA_real_)
  }

  # Convert to radians
  radians <- degrees * pi / 180

  # Calculate mean of sin and cos
  mean_sin <- mean(sin(radians), na.rm = TRUE)
  mean_cos <- mean(cos(radians), na.rm = TRUE)

  # Convert back to degrees
  mean_direction <- atan2(mean_sin, mean_cos) * 180 / pi

  # Normalize to 0-360
  if (mean_direction < 0) {
    mean_direction <- mean_direction + 360
  }

  return(round(mean_direction, 1))
}

# ------------------------------------------------------------------------------
#' Calculate Rolling Statistics
#'
#' Calculates rolling statistics for a numeric vector.
#' Optimized to use data.table's frollmean for performance.
#'
#' @param x Numeric vector
#' @param window Rolling window size
#' @param fun Function to apply (default: mean). Supports "mean", "sum", "min", "max"
#'
#' @return Numeric vector of rolling statistics
#' @export
# ------------------------------------------------------------------------------
rolling_stat <- function(x, window = 6, fun = mean) {
  n <- length(x)

  if (n < window) {
    return(rep(NA_real_, n))
  }

  # Use data.table's fast rolling functions when possible
  if (identical(fun, mean)) {
    return(frollmean(x, n = window, align = "right", na.rm = TRUE))
  } else if (identical(fun, sum)) {
    return(frollsum(x, n = window, align = "right", na.rm = TRUE))
  }

  # Fallback for custom functions - still much faster than the loop version
  # using vectorized operations
  result <- rep(NA_real_, n)

  for (i in window:n) {
    result[i] <- fun(x[(i - window + 1):i], na.rm = TRUE)
  }

  return(result)
}

# ------------------------------------------------------------------------------
#' Add Derived Columns to Observations
#'
#' Adds calculated fields like feels-like temp, dew point, wind cardinal.
#' Optimized to minimize function calls and use vectorized operations.
#'
#' @param df Data frame of observations
#'
#' @return Data frame with additional columns
#' @export
# ------------------------------------------------------------------------------
add_derived_columns <- function(df) {
  cat("\n  ==> add_derived_columns START\n")

  if (nrow(df) == 0) {
    cat("  ==> add_derived_columns: empty df, returning\n")
    return(df)
  }

  cat("  Input rows:", nrow(df), "\n")
  cat("  Columns:", paste(names(df), collapse = ", "), "\n")
  cat("  df class:", class(df), "\n")

  tryCatch(
    {
      cat("  Extracting wind_dir...\n")
      cat("    wind_dir class:", class(df$wind_dir), "\n")
      cat("    wind_dir length:", length(df$wind_dir), "\n")
      cat("    wind_dir sample:", head(df$wind_dir, 5), "\n")
      wind_dir_vec <- as.numeric(df$wind_dir)
      cat("    wind_dir_vec created, length:", length(wind_dir_vec), "\n")

      cat("  Extracting air_temp...\n")
      air_temp_vec <- as.numeric(df$air_temp)
      cat("    air_temp_vec length:", length(air_temp_vec), "\n")

      cat("  Extracting humidity...\n")
      humidity_vec <- as.numeric(df$humidity)
      cat("    humidity_vec length:", length(humidity_vec), "\n")

      cat("  Extracting wind_avg...\n")
      wind_avg_vec <- as.numeric(df$wind_avg)
      cat("    wind_avg_vec length:", length(wind_avg_vec), "\n")

      cat("  Extracting wind_gust...\n")
      wind_gust_vec <- as.numeric(df$wind_gust)
      cat("    wind_gust_vec length:", length(wind_gust_vec), "\n")

      cat("  Extracting precip...\n")
      precip_vec <- as.numeric(df$precip)
      cat("    precip_vec length:", length(precip_vec), "\n")

      cat("  Extracting lightning_count...\n")
      lightning_vec <- as.numeric(df$lightning_count)
      cat("    lightning_vec length:", length(lightning_vec), "\n")

      cat("  ALL EXTRACTIONS COMPLETE\n")
      cat("  Calling calculate_feels_like...\n")
      feels_like_calc <- calculate_feels_like(air_temp_vec, humidity_vec, wind_avg_vec)
      cat("    feels_like result length:", length(feels_like_calc), "\n")

      cat("  Calling calculate_dew_point...\n")
      dew_point_calc <- calculate_dew_point(air_temp_vec, humidity_vec)
      cat("    dew_point result length:", length(dew_point_calc), "\n")

      cat("  Calling get_wind_cardinal...\n")
      wind_cardinal_calc <- get_wind_cardinal(wind_dir_vec)
      cat("    wind_cardinal result length:", length(wind_cardinal_calc), "\n")

      cat("  ALL CALCULATIONS COMPLETE\n")
      cat("  Adding columns to dataframe...\n")

      df$feels_like <- feels_like_calc
      cat("    Added feels_like\n")
      df$dew_point <- dew_point_calc
      cat("    Added dew_point\n")
      df$wind_cardinal <- wind_cardinal_calc
      cat("    Added wind_cardinal\n")
      df$wind_avg_kph <- wind_avg_vec * 3.6
      cat("    Added wind_avg_kph\n")
      df$wind_gust_kph <- wind_gust_vec * 3.6
      cat("    Added wind_gust_kph\n")
      df$is_raining <- precip_vec > 0
      cat("    Added is_raining\n")
      df$has_lightning <- lightning_vec > 0
      cat("    Added has_lightning\n")

      cat("  ==> add_derived_columns END (SUCCESS), output rows:", nrow(df), "\n\n")
      return(df)
    },
    error = function(e) {
      cat("  *** ERROR in add_derived_columns:", e$message, "\n")
      cat("  *** Full error:", toString(e), "\n")
      cat("  ==> add_derived_columns END (ERROR)\n\n")
      stop(e)
    }
  )
}

# ------------------------------------------------------------------------------
#' Check Alert Conditions
#'
#' Evaluates current conditions against threshold settings.
#'
#' @param observation Single-row data frame of current observation
#' @param thresholds List of alert thresholds
#'
#' @return List of alert flags and messages
#' @export
# ------------------------------------------------------------------------------
check_alert_conditions <- function(observation, thresholds) {
  alerts <- list(
    has_alerts = FALSE,
    conditions = list()
  )

  if (nrow(observation) == 0) {
    return(alerts)
  }

  obs <- observation[1, ]

  # Check temperature extremes
  if (!is.na(obs$air_temp)) {
    if (obs$air_temp <= thresholds$extreme_cold_c) {
      alerts$conditions$extreme_cold <- list(
        active = TRUE,
        severity = "danger",
        message = paste("Extreme cold:", round(obs$air_temp, 1), "°C")
      )
    } else if (obs$air_temp <= thresholds$freezing_temp_c) {
      alerts$conditions$freezing <- list(
        active = TRUE,
        severity = "warning",
        message = paste("Freezing:", round(obs$air_temp, 1), "°C")
      )
    } else if (obs$air_temp >= thresholds$extreme_heat_c) {
      alerts$conditions$extreme_heat <- list(
        active = TRUE,
        severity = "danger",
        message = paste("Extreme heat:", round(obs$air_temp, 1), "°C")
      )
    } else if (obs$air_temp >= thresholds$heat_warning_c) {
      alerts$conditions$heat_warning <- list(
        active = TRUE,
        severity = "warning",
        message = paste("Heat warning:", round(obs$air_temp, 1), "°C")
      )
    }
  }

  # Check wind
  if (!is.na(obs$wind_gust)) {
    if (obs$wind_gust >= thresholds$extreme_wind_ms) {
      alerts$conditions$extreme_wind <- list(
        active = TRUE,
        severity = "danger",
        message = paste("Extreme wind gusts:", round(obs$wind_gust * 3.6, 1), "kph")
      )
    } else if (obs$wind_gust >= thresholds$high_wind_ms) {
      alerts$conditions$high_wind <- list(
        active = TRUE,
        severity = "warning",
        message = paste("High wind gusts:", round(obs$wind_gust * 3.6, 1), "kph")
      )
    }
  }

  # Check precipitation
  if (!is.na(obs$precip)) {
    if (obs$precip >= thresholds$extreme_rain_mm) {
      alerts$conditions$extreme_rain <- list(
        active = TRUE,
        severity = "danger",
        message = paste("Extreme rainfall:", round(obs$precip, 2), "mm")
      )
    } else if (obs$precip >= thresholds$heavy_rain_mm) {
      alerts$conditions$heavy_rain <- list(
        active = TRUE,
        severity = "warning",
        message = paste("Heavy rain:", round(obs$precip, 2), "mm")
      )
    }
  }

  # Check lightning
  if (!is.na(obs$lightning_count) && obs$lightning_count > thresholds$lightning_detected) {
    alerts$conditions$lightning <- list(
      active = TRUE,
      severity = "danger",
      message = paste("Lightning detected:", obs$lightning_count, "strikes")
    )
  }

  # Check battery
  if (!is.na(obs$battery) && obs$battery < thresholds$low_battery_v) {
    alerts$conditions$low_battery <- list(
      active = TRUE,
      severity = "warning",
      message = paste("Low battery:", round(obs$battery, 2), "V")
    )
  }

  alerts$has_alerts <- length(alerts$conditions) > 0

  return(alerts)
}

# ------------------------------------------------------------------------------
#' Calculate Summary Statistics
#'
#' Computes summary statistics for a time period.
#'
#' @param df Data frame of observations
#'
#' @return List of summary statistics
#' @export
# ------------------------------------------------------------------------------
calculate_summary_stats <- function(df) {
  if (nrow(df) == 0) {
    return(list(
      temp_min = NA, temp_max = NA, temp_avg = NA,
      humidity_min = NA, humidity_max = NA, humidity_avg = NA,
      wind_max = NA, wind_avg = NA,
      precip_total = NA,
      lightning_total = NA,
      pressure_min = NA, pressure_max = NA
    ))
  }

  list(
    temp_min = min(df$air_temp, na.rm = TRUE),
    temp_max = max(df$air_temp, na.rm = TRUE),
    temp_avg = mean(df$air_temp, na.rm = TRUE),
    humidity_min = min(df$humidity, na.rm = TRUE),
    humidity_max = max(df$humidity, na.rm = TRUE),
    humidity_avg = mean(df$humidity, na.rm = TRUE),
    wind_max = max(df$wind_gust, na.rm = TRUE) * 3.6,
    # Convert to kph
    wind_avg = mean(df$wind_avg, na.rm = TRUE) * 3.6,
    precip_total = sum(df$precip, na.rm = TRUE),
    lightning_total = sum(df$lightning_count, na.rm = TRUE),
    pressure_min = min(df$pressure, na.rm = TRUE),
    pressure_max = max(df$pressure, na.rm = TRUE)
  )
}

# ------------------------------------------------------------------------------
#' Prepare Data for Plotting
#'
#' Prepares observation data for visualization with proper formatting.
#'
#' @param df Data frame of observations
#' @param display_tz Display timezone
#'
#' @return Formatted data frame ready for plotting
#' @export
# ------------------------------------------------------------------------------
prepare_plot_data <- function(df, display_tz = "America/New_York") {
  cat("\n>>> PREPARE_PLOT_DATA START <<<\n")
  cat("Input rows:", nrow(df), "\n")

  if (nrow(df) == 0) {
    cat("Empty dataframe, returning\n")
    cat(">>> PREPARE_PLOT_DATA END <<<\n\n")
    return(df)
  }

  tryCatch(
    {
      cat("Step 1: convert_to_display_tz...\n")
      df <- convert_to_display_tz(df, display_tz = display_tz)
      cat("Step 1 complete. Rows:", nrow(df), "\n")

      cat("Step 2: add_derived_columns...\n")
      df <- add_derived_columns(df)
      cat("Step 2 complete. Rows:", nrow(df), "\n")

      cat("Step 3: arrange by timestamp...\n")
      df <- df |> arrange(timestamp)
      cat("Step 3 complete. Rows:", nrow(df), "\n")

      cat(">>> PREPARE_PLOT_DATA END (SUCCESS) <<<\n\n")
      return(df)
    },
    error = function(e) {
      cat("ERROR in prepare_plot_data:", e$message, "\n")
      cat(">>> PREPARE_PLOT_DATA END (ERROR) <<<\n\n")
      stop(e)
    }
  )
}
