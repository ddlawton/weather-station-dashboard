# ==============================================================================
# db_access.R
# Database access layer for Weather Station Dashboard
# Handles all PostgreSQL connections and queries using connection pooling
# ==============================================================================

library(pool)
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)

# ------------------------------------------------------------------------------
#' Create Database Connection Pool
#'
#' Creates a pooled connection to the PostgreSQL database for efficient
#' connection management in a Shiny application context.
#'
#' @param config List containing database configuration (host, port, dbname,
#'   user, password)
#' @param min_size Minimum number of connections in pool (default: 1)
#' @param max_size Maximum number of connections in pool (default: 5)
#'
#' @return A pool object for database connections
#' @export
# ------------------------------------------------------------------------------
create_db_pool <- function(config, min_size = 1, max_size = 5) {
  tryCatch(
    {
      pool <- dbPool(
        drv = RPostgres::Postgres(),
        host = config$host,
        port = config$port,
        dbname = config$dbname,
        user = config$user,
        password = config$password,
        minSize = min_size,
        maxSize = max_size,
        idleTimeout = 60000
        # Close idle connections after 1 minute
      )

      # Test connection
      test_conn <- poolCheckout(pool)
      poolReturn(test_conn)

      message("Database connection pool created successfully.")
      return(pool)
    },
    error = function(e) {
      warning(paste("Failed to create database pool:", e$message))
      return(NULL)
    }
  )
}

# ------------------------------------------------------------------------------
#' Close Database Pool
#'
#' Safely closes all connections in the pool.
#'
#' @param pool The pool object to close
#'
#' @return NULL (invisible)
#' @export
# ------------------------------------------------------------------------------
close_db_pool <- function(pool) {
  if (!is.null(pool)) {
    tryCatch(
      {
        poolClose(pool)
        message("Database pool closed successfully.")
      },
      error = function(e) {
        warning(paste("Error closing pool:", e$message))
      }
    )
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------
#' Check Database Connection Status
#'
#' Tests if the database pool is valid and connections are available.
#'
#' @param pool The pool object to test
#'
#' @return Logical TRUE if connection is valid, FALSE otherwise
#' @export
# ------------------------------------------------------------------------------
check_db_connection <- function(pool) {
  if (is.null(pool)) {
    return(FALSE)
  }

  tryCatch(
    {
      result <- dbGetQuery(pool, "SELECT 1 AS test")
      return(nrow(result) == 1)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# ------------------------------------------------------------------------------
#' Fetch Weather Observations
#'
#' Retrieves weather observations from the obs_st table for a specified
#' time range. Optionally aggregates data at the database level for better
#' performance with large time ranges.
#'
#' @param pool Database connection pool
#' @param start_time POSIXct start time (UTC)
#' @param end_time POSIXct end time (UTC), defaults to current time
#' @param station_id Station identifier (default: NULL for all stations)
#' @param aggregate_interval Optional aggregation interval (e.g., "15 minutes",
#'   "1 hour"). If provided, aggregation is done in the database for better performance.
#'
#' @return A tibble of weather observations
#' @export
# ------------------------------------------------------------------------------
fetch_observations <- function(pool,
                               start_time,
                               end_time = Sys.time(),
                               station_id = NULL,
                               aggregate_interval = NULL) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(tibble())
  }

  # If aggregation is requested, do it in the database (much faster)
  if (!is.null(aggregate_interval)) {
    return(fetch_observations_aggregated(
      pool, start_time, end_time,
      station_id, aggregate_interval
    ))
  }

  # Build query with parameterized inputs for security
  base_query <- "
    SELECT
      id,
      timestamp,
      station_id,
      wind_lull,
      wind_avg,
      wind_gust,
      wind_dir,
      pressure,
      air_temp,
      humidity,
      illuminance,
      uv,
      solar_rad,
      precip,
      precip_type,
      lightning_dist,
      lightning_count,
      battery
    FROM obs_st
    WHERE timestamp >= $1
      AND timestamp <= $2
  "

  if (!is.null(station_id)) {
    base_query <- paste(base_query, "AND station_id = $3")
  }

  base_query <- paste(base_query, "ORDER BY timestamp DESC")

  tryCatch(
    {
      if (!is.null(station_id)) {
        result <- dbGetQuery(
          pool,
          base_query,
          params = list(start_time, end_time, station_id)
        )
      } else {
        result <- dbGetQuery(
          pool,
          base_query,
          params = list(start_time, end_time)
        )
      }

      return(as_tibble(result))
    },
    error = function(e) {
      warning(paste("Error fetching observations:", e$message))
      return(tibble())
    }
  )
}

# ------------------------------------------------------------------------------
#' Fetch Weather Observations with Database-side Aggregation
#'
#' Internal function that performs aggregation in PostgreSQL for better performance.
#'
#' @param pool Database connection pool
#' @param start_time POSIXct start time (UTC)
#' @param end_time POSIXct end time (UTC)
#' @param station_id Station identifier (optional)
#' @param interval Aggregation interval (e.g., "15 minutes", "1 hour")
#'
#' @return A tibble of aggregated observations
# ------------------------------------------------------------------------------
fetch_observations_aggregated <- function(pool, start_time, end_time,
                                          station_id = NULL, interval) {
  # Build aggregation query with time_bucket for PostgreSQL
  base_query <- paste0("
    SELECT
      time_bucket('", interval, "', timestamp) AS timestamp,
      station_id,
      MIN(wind_lull) AS wind_lull,
      AVG(wind_avg) AS wind_avg,
      MAX(wind_gust) AS wind_gust,
      -- Circular mean for wind direction approximation
      DEGREES(ATAN2(AVG(SIN(RADIANS(wind_dir))), AVG(COS(RADIANS(wind_dir))))) AS wind_dir,
      AVG(pressure) AS pressure,
      AVG(air_temp) AS air_temp,
      AVG(humidity) AS humidity,
      AVG(illuminance) AS illuminance,
      AVG(uv) AS uv,
      AVG(solar_rad) AS solar_rad,
      SUM(precip) AS precip,
      MAX(precip_type) AS precip_type,
      AVG(lightning_dist) AS lightning_dist,
      SUM(lightning_count) AS lightning_count,
      AVG(battery) AS battery,
      COUNT(*) AS observation_count
    FROM obs_st
    WHERE timestamp >= $1
      AND timestamp <= $2
  ")

  if (!is.null(station_id)) {
    base_query <- paste(base_query, "AND station_id = $3")
  }

  base_query <- paste(base_query, "GROUP BY 1, station_id ORDER BY timestamp DESC")

  tryCatch(
    {
      if (!is.null(station_id)) {
        result <- dbGetQuery(pool, base_query,
          params = list(start_time, end_time, station_id)
        )
      } else {
        result <- dbGetQuery(pool, base_query,
          params = list(start_time, end_time)
        )
      }

      # Normalize wind direction to 0-360
      if (nrow(result) > 0 && "wind_dir" %in% names(result)) {
        result$wind_dir <- (result$wind_dir + 360) %% 360
      }

      return(as_tibble(result))
    },
    error = function(e) {
      # If time_bucket is not available (not using TimescaleDB), fall back to R aggregation
      warning(paste("Database aggregation failed, using R aggregation:", e$message))

      result <- fetch_observations(pool, start_time, end_time, station_id,
        aggregate_interval = NULL
      )
      if (nrow(result) > 0) {
        # Use R aggregation as fallback
        result <- aggregate_observations(result, interval = interval)
      }
      return(result)
    }
  )
}

# ------------------------------------------------------------------------------
#' Fetch Latest Observation
#'
#' Retrieves the most recent weather observation.
#'
#' @param pool Database connection pool
#' @param station_id Station identifier (optional)
#'
#' @return A single-row tibble with the latest observation
#' @export
# ------------------------------------------------------------------------------
fetch_latest_observation <- function(pool, station_id = NULL) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(tibble())
  }

  query <- "
    SELECT
      id,
      timestamp,
      station_id,
      wind_lull,
      wind_avg,
      wind_gust,
      wind_dir,
      pressure,
      air_temp,
      humidity,
      illuminance,
      uv,
      solar_rad,
      precip,
      precip_type,
      lightning_dist,
      lightning_count,
      battery
    FROM obs_st
  "

  if (!is.null(station_id)) {
    query <- paste(query, "WHERE station_id = $1")
    query <- paste(query, "ORDER BY timestamp DESC LIMIT 1")

    tryCatch(
      {
        result <- dbGetQuery(pool, query, params = list(station_id))
        return(as_tibble(result))
      },
      error = function(e) {
        warning(paste("Error fetching latest observation:", e$message))
        return(tibble())
      }
    )
  } else {
    query <- paste(query, "ORDER BY timestamp DESC LIMIT 1")

    tryCatch(
      {
        result <- dbGetQuery(pool, query)
        return(as_tibble(result))
      },
      error = function(e) {
        warning(paste("Error fetching latest observation:", e$message))
        return(tibble())
      }
    )
  }
}

# ------------------------------------------------------------------------------
#' Fetch Rapid Wind Data
#'
#' Retrieves minute-aggregated wind data from rapid_wind_1min table.
#'
#' @param pool Database connection pool
#' @param start_time POSIXct start time (UTC)
#' @param end_time POSIXct end time (UTC)
#' @param station_id Station identifier (optional)
#'
#' @return A tibble of rapid wind data
#' @export
# ------------------------------------------------------------------------------
fetch_rapid_wind <- function(pool,
                             start_time,
                             end_time = Sys.time(),
                             station_id = NULL) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(tibble())
  }

  query <- "
    SELECT
      id,
      minute_timestamp,
      station_id,
      wind_speed_avg,
      wind_speed_max,
      wind_speed_min,
      wind_dir_avg,
      sample_count
    FROM rapid_wind_1min
    WHERE minute_timestamp >= $1
      AND minute_timestamp <= $2
  "

  if (!is.null(station_id)) {
    query <- paste(query, "AND station_id = $3")
  }

  query <- paste(query, "ORDER BY minute_timestamp DESC")

  tryCatch(
    {
      if (!is.null(station_id)) {
        result <- dbGetQuery(
          pool,
          query,
          params = list(start_time, end_time, station_id)
        )
      } else {
        result <- dbGetQuery(
          pool,
          query,
          params = list(start_time, end_time)
        )
      }

      return(as_tibble(result))
    },
    error = function(e) {
      warning(paste("Error fetching rapid wind data:", e$message))
      return(tibble())
    }
  )
}

# ------------------------------------------------------------------------------
#' Fetch Device Status
#'
#' Retrieves device status information.
#'
#' @param pool Database connection pool
#' @param station_id Station identifier (optional)
#' @param limit Number of records to retrieve (default: 100)
#'
#' @return A tibble of device status records
#' @export
# ------------------------------------------------------------------------------
fetch_device_status <- function(pool, station_id = NULL, limit = 100) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(tibble())
  }

  query <- "
    SELECT
      id,
      timestamp,
      station_id,
      uptime,
      voltage,
      firmware_revision,
      rssi,
      hub_rssi,
      sensor_status
    FROM device_status
  "

  if (!is.null(station_id)) {
    query <- paste(query, "WHERE station_id = $1")
    query <- paste(query, "ORDER BY timestamp DESC LIMIT", limit)

    tryCatch(
      {
        result <- dbGetQuery(pool, query, params = list(station_id))
        return(as_tibble(result))
      },
      error = function(e) {
        warning(paste("Error fetching device status:", e$message))
        return(tibble())
      }
    )
  } else {
    query <- paste(query, "ORDER BY timestamp DESC LIMIT", limit)

    tryCatch(
      {
        result <- dbGetQuery(pool, query)
        return(as_tibble(result))
      },
      error = function(e) {
        warning(paste("Error fetching device status:", e$message))
        return(tibble())
      }
    )
  }
}

# ------------------------------------------------------------------------------
#' Fetch Hub Status
#'
#' Retrieves hub status information.
#'
#' @param pool Database connection pool
#' @param hub_sn Hub serial number (optional)
#' @param limit Number of records to retrieve (default: 100)
#'
#' @return A tibble of hub status records
#' @export
# ------------------------------------------------------------------------------
fetch_hub_status <- function(pool, hub_sn = NULL, limit = 100) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(tibble())
  }

  query <- "
    SELECT
      id,
      timestamp,
      hub_sn,
      uptime,
      firmware_revision,
      rssi,
      reset_flags,
      seq
    FROM hub_status
  "

  if (!is.null(hub_sn)) {
    query <- paste(query, "WHERE hub_sn = $1")
    query <- paste(query, "ORDER BY timestamp DESC LIMIT", limit)

    tryCatch(
      {
        result <- dbGetQuery(pool, query, params = list(hub_sn))
        return(as_tibble(result))
      },
      error = function(e) {
        warning(paste("Error fetching hub status:", e$message))
        return(tibble())
      }
    )
  } else {
    query <- paste(query, "ORDER BY timestamp DESC LIMIT", limit)

    tryCatch(
      {
        result <- dbGetQuery(pool, query)
        return(as_tibble(result))
      },
      error = function(e) {
        warning(paste("Error fetching hub status:", e$message))
        return(tibble())
      }
    )
  }
}

# ------------------------------------------------------------------------------
#' Fetch Recent Lightning Events
#'
#' Retrieves count of lightning strikes in the specified time window.
#'
#' @param pool Database connection pool
#' @param hours Number of hours to look back (default: 24)
#' @param station_id Station identifier (optional)
#'
#' @return Integer count of lightning strikes
#' @export
# ------------------------------------------------------------------------------
fetch_lightning_count <- function(pool, hours = 24, station_id = NULL) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(0)
  }

  start_time <- Sys.time() - hours(hours)

  query <- "
    SELECT COALESCE(SUM(lightning_count), 0) AS total_strikes
    FROM obs_st
    WHERE timestamp >= $1
  "

  if (!is.null(station_id)) {
    query <- paste(query, "AND station_id = $2")

    tryCatch(
      {
        result <- dbGetQuery(pool, query, params = list(start_time, station_id))
        return(as.integer(result$total_strikes[1]))
      },
      error = function(e) {
        warning(paste("Error fetching lightning count:", e$message))
        return(0)
      }
    )
  } else {
    tryCatch(
      {
        result <- dbGetQuery(pool, query, params = list(start_time))
        return(as.integer(result$total_strikes[1]))
      },
      error = function(e) {
        warning(paste("Error fetching lightning count:", e$message))
        return(0)
      }
    )
  }
}

# ------------------------------------------------------------------------------
#' Fetch Precipitation Total
#'
#' Calculates total precipitation for a time window.
#'
#' @param pool Database connection pool
#' @param hours Number of hours to look back
#' @param station_id Station identifier (optional)
#'
#' @return Numeric total precipitation in mm
#' @export
# ------------------------------------------------------------------------------
fetch_precipitation_total <- function(pool, hours = 24, station_id = NULL) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(0)
  }

  start_time <- Sys.time() - hours(hours)

  query <- "
    SELECT COALESCE(SUM(precip), 0) AS total_precip
    FROM obs_st
    WHERE timestamp >= $1
  "

  if (!is.null(station_id)) {
    query <- paste(query, "AND station_id = $2")

    tryCatch(
      {
        result <- dbGetQuery(pool, query, params = list(start_time, station_id))
        return(as.numeric(result$total_precip[1]))
      },
      error = function(e) {
        warning(paste("Error fetching precipitation total:", e$message))
        return(0)
      }
    )
  } else {
    tryCatch(
      {
        result <- dbGetQuery(pool, query, params = list(start_time))
        return(as.numeric(result$total_precip[1]))
      },
      error = function(e) {
        warning(paste("Error fetching precipitation total:", e$message))
        return(0)
      }
    )
  }
}

# ------------------------------------------------------------------------------
#' Get Available Stations
#'
#' Retrieves list of unique station IDs in the database.
#'
#' @param pool Database connection pool
#'
#' @return Character vector of station IDs
#' @export
# ------------------------------------------------------------------------------
get_available_stations <- function(pool) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(character())
  }

  query <- "SELECT DISTINCT station_id FROM obs_st ORDER BY station_id"

  tryCatch(
    {
      result <- dbGetQuery(pool, query)
      return(result$station_id)
    },
    error = function(e) {
      warning(paste("Error fetching stations:", e$message))
      return(character())
    }
  )
}

# ------------------------------------------------------------------------------
#' Get Data Time Range
#'
#' Returns the earliest and latest timestamps in the database.
#'
#' @param pool Database connection pool
#'
#' @return List with 'min' and 'max' POSIXct timestamps
#' @export
# ------------------------------------------------------------------------------
get_data_time_range <- function(pool) {
  if (is.null(pool)) {
    warning("Database pool is NULL")
    return(list(min = NA, max = NA))
  }

  query <- "
    SELECT
      MIN(timestamp) AS min_time,
      MAX(timestamp) AS max_time
    FROM obs_st
  "

  tryCatch(
    {
      result <- dbGetQuery(pool, query)
      return(list(
        min = result$min_time[1],
        max = result$max_time[1]
      ))
    },
    error = function(e) {
      warning(paste("Error fetching time range:", e$message))
      return(list(min = NA, max = NA))
    }
  )
}
