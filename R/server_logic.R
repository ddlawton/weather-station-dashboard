# ==============================================================================
# server_logic.R
# Server logic for Weather Station Dashboard
# Handles data fetching, reactivity, and output rendering
# ==============================================================================

library(shiny)
library(lubridate)
library(dplyr)

# ------------------------------------------------------------------------------
#' Build Server Logic
#'
#' Constructs the Shiny server function with all reactive elements.
#'
#' @param pool Database connection pool (passed from app.R)
#' @param noaa_pool NOAA database connection pool (optional)
#'
#' @return A Shiny server function
#' @export
# ------------------------------------------------------------------------------
build_server <- function(pool, noaa_pool = NULL) {
  function(input, output, session) {
    # ==========================================================================
    # REACTIVE VALUES
    # ==========================================================================

    # Store reactive data
    rv <- reactiveValues(
      observations = NULL,
      latest_obs = NULL,
      rapid_wind = NULL,
      device_status = NULL,
      hub_status = NULL,
      alerts = NULL,
      last_update = NULL,
      db_connected = FALSE,
      precip_24h = 0,
      lightning_24h = 0
    )

    # ==========================================================================
    # DATABASE CONNECTION CHECK
    # ==========================================================================

    # Check database connection on startup
    observe({
      rv$db_connected <- check_db_connection(pool)

      if (!rv$db_connected) {
        showNotification(
          "Unable to connect to the weather database. Please check the connection.",
          type = "error",
          duration = NULL
        )
      }
    })

    noaa_connected <- reactive({
      check_db_connection(noaa_pool)
    })

    # ==========================================================================
    # STATION INITIALIZATION
    # ==========================================================================

    # Populate station selector
    observe({
      if (rv$db_connected) {
        stations <- get_available_stations(pool)

        if (length(stations) > 0) {
          updateSelectInput(
            session,
            "station_selector",
            choices = stations,
            selected = stations[1]
          )
        }
      }
    })

    # Get selected station
    selected_station <- reactive({
      req(input$station_selector)
      input$station_selector
    })

    # ==========================================================================
    # SIDEBAR TAB SWITCHING
    # ==========================================================================

    # Toggle unified-specific controls based on active tab
    observe({
      active_tab <- input$main_tabs
      is_unified <- identical(active_tab, "unified")

      if (is_unified) {
        shinyjs::show("unified_controls_sidebar")
        shinyjs::runjs("document.querySelector('.main-sidebar').setAttribute('data-unified-active', 'true');")
      } else {
        shinyjs::hide("unified_controls_sidebar")
        shinyjs::runjs("document.querySelector('.main-sidebar').setAttribute('data-unified-active', 'false');")
      }
    })

    # ==========================================================================
    # TIME WINDOW CALCULATION
    # ==========================================================================

    # Calculate time range based on selected window
    time_range <- reactive({
      window_key <- input$time_window
      if (is.null(window_key)) window_key <- "24h"

      hours <- time_windows[[window_key]]$hours

      end_time <- Sys.time()

      # Handle "all time" option
      if (is.null(hours)) {
        # For all time, query the earliest data point (set far back)
        start_time <- end_time - years(10)
        hours <- 87600 # 10 years in hours for aggregation logic
      } else {
        start_time <- end_time - hours(hours)
      }

      # Determine aggregation interval based on time range
      aggregate_interval <- "15 min"
      if (hours > 8760) {
        aggregate_interval <- "1 day"
      } else if (hours > 4320) {
        aggregate_interval <- "12 hours"
      } else if (hours > 2160) {
        aggregate_interval <- "6 hours"
      } else if (hours > 720) {
        aggregate_interval <- "1 hour"
      } else if (hours > 168) {
        aggregate_interval <- "15 min"
      }

      list(
        start = start_time,
        end = end_time,
        hours = hours,
        aggregate_interval = aggregate_interval
      )
    })

    # ==========================================================================
    # DATA FETCHING
    # ==========================================================================

    # Fetch current conditions (every minute)
    observe({
      # Invalidate every minute for current conditions
      invalidateLater(refresh_intervals$current_conditions, session)

      if (!rv$db_connected) {
        return()
      }

      tryCatch(
        {
          # Get latest observation
          rv$latest_obs <- fetch_latest_observation(pool, selected_station())

          # Get 24h totals
          rv$precip_24h <- fetch_precipitation_total(pool, 24, selected_station())
          rv$lightning_24h <- fetch_lightning_count(pool, 24, selected_station())

          # Get device/hub status
          rv$device_status <- fetch_device_status(pool, selected_station(), limit = 1)
          rv$hub_status <- fetch_hub_status(pool, limit = 1)

          # Check alert conditions
          if (nrow(rv$latest_obs) > 0) {
            rv$alerts <- check_alert_conditions(rv$latest_obs, alert_thresholds)
          }

          rv$last_update <- Sys.time()
        },
        error = function(e) {
          showNotification(
            paste("Error fetching current conditions:", e$message),
            type = "warning",
            duration = 10
          )
        }
      )
    })

    # Fetch historical data (based on user-selected refresh rate or 6 hours)
    historical_data <- reactive({
      # Refresh based on user setting or button click
      input$refresh_now

      # Auto-refresh based on selected rate
      refresh_rate <- as.numeric(input$refresh_rate)
      if (refresh_rate > 0) {
        invalidateLater(refresh_rate, session)
      }

      req(rv$db_connected)
      req(time_range())

      range <- time_range()

      tryCatch(
        {
          # Use database-side aggregation for better performance on large time ranges
          # Only aggregate if time window is longer than 48 hours
          if (range$hours > 48) {
            obs <- fetch_observations(
              pool,
              start_time = range$start,
              end_time = range$end,
              station_id = selected_station(),
              aggregate_interval = range$aggregate_interval
            )
          } else {
            # For shorter time ranges, get raw data
            obs <- fetch_observations(
              pool,
              start_time = range$start,
              end_time = range$end,
              station_id = selected_station()
            )
          }

          if (nrow(obs) > 0) {
            # Prepare data for plotting (timezone conversion and derived columns)
            obs <- prepare_plot_data(obs, timezone_display)
          }

          obs
        },
        error = function(e) {
          showNotification(
            paste("Error fetching historical data:", e$message),
            type = "warning",
            duration = 10
          )
          tibble()
        }
      )
    })

    # Fetch rapid wind data for wind rose
    rapid_wind_data <- reactive({
      input$refresh_now

      req(rv$db_connected)
      req(time_range())

      range <- time_range()

      tryCatch(
        {
          fetch_rapid_wind(
            pool,
            start_time = range$start,
            end_time = range$end,
            station_id = selected_station()
          )
        },
        error = function(e) {
          tibble()
        }
      )
    })

    # ========================================================================
    # HELPER FUNCTION FOR DATA FETCHING
    # ========================================================================

    # Standardized fetch with error handling - reduces duplication
    safe_fetch <- function(fetch_fn, ...) {
      tryCatch(
        fetch_fn(...),
        error = function(e) tibble()
      )
    }

    # ========================================================================
    # UNIFIED VIEW DATA
    # ========================================================================

    unified_tempest_raw <- reactive({
      input$refresh_now
      req(rv$db_connected, time_range())
      range <- time_range()
      safe_fetch(fetch_observations, pool, range$start, range$end, selected_station(), NULL)
    })

    unified_tempest_hourly <- reactive({
      raw <- unified_tempest_raw()
      if (nrow(raw) == 0) {
        return(tibble())
      }
      aggregate_tempest_hourly(raw)
    })

    unified_noaa_obs_raw <- reactive({
      input$refresh_now
      req(time_range(), noaa_connected())
      range <- time_range()
      source <- input$unified_noaa_source %||% "NWS"
      safe_fetch(fetch_noaa_observations, noaa_pool, range$start, range$end, "KRDU", source)
    })

    unified_noaa_obs_hourly <- reactive({
      raw <- unified_noaa_obs_raw()
      if (nrow(raw) == 0) {
        return(tibble())
      }
      aggregate_noaa_obs_hourly(raw, station_id = "KRDU")
    })

    unified_noaa_forecast_raw <- reactive({
      input$refresh_now
      req(time_range(), noaa_connected())
      range <- time_range()
      safe_fetch(fetch_noaa_forecast_hourly, noaa_pool, range$start, range$end, "KRDU")
    })

    unified_noaa_forecast_hourly <- reactive({
      raw <- unified_noaa_forecast_raw()
      if (nrow(raw) == 0) {
        return(tibble())
      }
      prepare_noaa_forecast_hourly(raw, station_id = "KRDU")
    })

    noaa_tab_obs_raw <- reactive({
      input$refresh_now
      refresh_rate <- as.numeric(input$refresh_rate)
      if (!is.na(refresh_rate) && refresh_rate > 0) {
        invalidateLater(refresh_rate, session)
      }

      req(noaa_connected(), time_range())
      range <- time_range()

      source_filter <- input$noaa_source_filter %||% "NWS"
      source_value <- if (identical(source_filter, "all")) NULL else source_filter

      safe_fetch(
        fetch_noaa_observations,
        pool = noaa_pool,
        start_time = range$start,
        end_time = range$end,
        station_id = "KRDU",
        data_source = source_value
      )
    })

    noaa_tab_obs_hourly <- reactive({
      raw <- noaa_tab_obs_raw()
      if (nrow(raw) == 0) {
        return(tibble())
      }
      aggregate_noaa_obs_hourly(raw, station_id = "KRDU")
    })

    forecast_fetch_window <- reactive({
      horizon <- input$forecast_horizon_hours
      if (is.null(horizon) || length(horizon) != 2) {
        horizon <- c(12, 168)
      }

      list(
        start = Sys.time() - hours(24),
        end = Sys.time() + hours(max(horizon, na.rm = TRUE) + 24)
      )
    })

    forecast_tab_raw <- reactive({
      input$refresh_now
      refresh_rate <- as.numeric(input$refresh_rate)
      if (!is.na(refresh_rate) && refresh_rate > 0) {
        invalidateLater(refresh_rate, session)
      }

      req(noaa_connected(), forecast_fetch_window())
      window <- forecast_fetch_window()

      safe_fetch(
        fetch_noaa_forecast_hourly,
        pool = noaa_pool,
        start_time = window$start,
        end_time = window$end,
        station_id = "KRDU"
      )
    })

    forecast_tab_hourly <- reactive({
      raw <- forecast_tab_raw()
      if (nrow(raw) == 0) {
        return(tibble())
      }

      data <- prepare_noaa_forecast_hourly(raw, station_id = "KRDU")
      horizon <- input$forecast_horizon_hours
      if (!is.null(horizon) && length(horizon) == 2) {
        data <- data |>
          filter(lead_hours >= horizon[1], lead_hours <= horizon[2])
      }

      data
    })

    openmeteo_forecast_raw <- reactive({
      input$refresh_now
      refresh_rate <- as.numeric(input$refresh_rate)
      if (!is.na(refresh_rate) && refresh_rate > 0) {
        invalidateLater(refresh_rate, session)
      }

      window <- forecast_fetch_window()

      safe_fetch(
        fetch_openmeteo_forecast_hourly,
        latitude = 35.8776,
        longitude = -78.7875,
        start_time = window$start,
        end_time = window$end,
        timezone = "UTC"
      )
    })

    openmeteo_forecast_hourly <- reactive({
      raw <- openmeteo_forecast_raw()
      if (nrow(raw) == 0) {
        return(tibble())
      }

      horizon <- input$forecast_horizon_hours
      if (is.null(horizon) || length(horizon) != 2) {
        horizon <- c(12, 168)
      }

      raw |>
        mutate(
          forecast_hour = floor_date(forecast_time, unit = "hour"),
          generated_hour = floor_date(generated_at, unit = "hour"),
          lead_hours = as.numeric(difftime(forecast_hour, generated_hour, units = "hours")),
          om_fcst_temperature_c = temperature,
          om_fcst_humidity = relative_humidity,
          om_fcst_pressure_hpa = pressure,
          om_fcst_wind_speed = wind_speed,
          om_fcst_wind_gust = wind_gust,
          om_fcst_precip_probability = precipitation_probability,
          om_weather_code = weather_code
        ) |>
        filter(lead_hours >= horizon[1], lead_hours <= horizon[2]) |>
        select(
          forecast_hour,
          generated_hour,
          lead_hours,
          starts_with("om_fcst_"),
          om_weather_code,
          latitude,
          longitude
        )
    })

    unified_obs_comparison <- reactive({
      variable_name <- input$unified_variable
      if (is.null(variable_name)) {
        variable_name <- "temperature"
      }

      build_obs_comparison_hourly(
        unified_noaa_obs_hourly(),
        unified_tempest_hourly(),
        variable_name = variable_name
      )
    })

    unified_forecast_accuracy <- reactive({
      variable_name <- input$unified_variable
      baseline <- input$unified_baseline

      if (is.null(variable_name)) variable_name <- "temperature"
      if (is.null(baseline)) baseline <- "noaa_obs"

      data <- build_forecast_accuracy_hourly(
        unified_noaa_forecast_hourly(),
        unified_noaa_obs_hourly(),
        unified_tempest_hourly(),
        baseline = baseline,
        variable_name = variable_name
      )

      lead_window <- input$unified_lead_hours
      if (!is.null(lead_window) && length(lead_window) == 2) {
        data <- data |>
          filter(lead_hours >= lead_window[1], lead_hours <= lead_window[2])
      }

      data
    })

    unified_target_hours <- reactive({
      data <- unified_forecast_accuracy()
      if (nrow(data) == 0) {
        return(character(0))
      }

      format(sort(unique(data$forecast_hour), decreasing = TRUE), "%Y-%m-%d %H:%M:%S")
    })

    output$unified_target_time_selector <- renderUI({
      choices <- unified_target_hours()

      if (length(choices) == 0) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"),
            " No forecast target hours available for current filters."
          )
        )
      }

      selectInput(
        inputId = "unified_target_hour",
        label = "Forecast Target Hour",
        choices = choices,
        selected = choices[1]
      )
    })

    # ==========================================================================
    # UI OUTPUTS - NOAA / FORECAST TABS
    # ==========================================================================

    variable_meta <- function(variable, units_system = "metric") {
      label_map <- c(
        temperature = "Temperature",
        humidity = "Humidity",
        pressure = "Pressure",
        wind_avg = "Wind (Average)",
        wind_gust = "Wind (Gust)"
      )

      unit_metric <- c(
        temperature = "°C",
        humidity = "%",
        pressure = "hPa",
        wind_avg = "m/s",
        wind_gust = "m/s"
      )

      unit_us <- c(
        temperature = "°F",
        humidity = "%",
        pressure = "hPa",
        wind_avg = "mph",
        wind_gust = "mph"
      )

      obs_col_map <- c(
        temperature = "noaa_obs_temperature_c",
        humidity = "noaa_obs_humidity",
        pressure = "noaa_obs_pressure_hpa",
        wind_avg = "noaa_obs_wind_speed",
        wind_gust = "noaa_obs_wind_gust"
      )

      fcst_col_map <- c(
        temperature = "noaa_fcst_temperature_c",
        humidity = "noaa_fcst_humidity",
        pressure = "noaa_fcst_pressure_hpa",
        wind_avg = "noaa_fcst_wind_speed",
        wind_gust = "noaa_fcst_wind_gust"
      )

      fcst_col_map_openmeteo <- c(
        temperature = "om_fcst_temperature_c",
        humidity = "om_fcst_humidity",
        pressure = "om_fcst_pressure_hpa",
        wind_avg = "om_fcst_wind_speed",
        wind_gust = "om_fcst_wind_gust"
      )

      list(
        variable = variable,
        label = label_map[[variable]],
        unit = if (identical(units_system, "us")) unit_us[[variable]] else unit_metric[[variable]],
        obs_col = obs_col_map[[variable]],
        fcst_col = fcst_col_map[[variable]],
        fcst_col_openmeteo = fcst_col_map_openmeteo[[variable]]
      )
    }

    resolve_forecast_column <- function(info, provider = "noaa") {
      if (identical(provider, "openmeteo")) {
        return(info$fcst_col_openmeteo)
      }
      info$fcst_col
    }

    convert_variable_values <- function(values, variable, units_system) {
      if (!identical(units_system, "us")) {
        return(values)
      }

      if (identical(variable, "temperature")) {
        return(convert_temperature(values, from = "C", to = "F"))
      }
      if (identical(variable, "wind_avg") || identical(variable, "wind_gust")) {
        return(convert_wind_speed(values, to = "mph"))
      }

      values
    }

    output$noaa_data_status <- renderUI({
      if (isTRUE(noaa_connected())) {
        div(class = "text-success", icon("check-circle"), " NOAA source connected")
      } else {
        div(class = "text-danger", icon("exclamation-triangle"), " NOAA source unavailable")
      }
    })

    output$noaa_summary_boxes <- renderUI({
      raw <- noaa_tab_obs_raw()
      hourly <- noaa_tab_obs_hourly()

      total_rows <- nrow(raw)
      latest_ts <- if (total_rows > 0) max(raw$observation_time, na.rm = TRUE) else NA
      source_count <- if (total_rows > 0 && "data_source" %in% names(raw)) dplyr::n_distinct(raw$data_source) else 0
      hourly_points <- nrow(hourly)

      tagList(
        weather_value_box(total_rows, "NOAA Rows", icon = "database", color = "info", width = 3),
        weather_value_box(
          if (is.na(latest_ts)) "--" else format(with_tz(latest_ts, timezone_display), "%m-%d %H:%M"),
          "Latest NOAA Obs",
          icon = "clock",
          color = "pressure",
          width = 3
        ),
        weather_value_box(source_count, "Active Sources", icon = "stream", color = "humidity", width = 3),
        weather_value_box(hourly_points, "Hourly Buckets", icon = "th", color = "wind", width = 3)
      )
    })

    output$noaa_observation_plot <- renderPlotly({
      hourly <- noaa_tab_obs_hourly()
      variable <- input$noaa_variable %||% "temperature"
      units <- input$units_system %||% "metric"
      info <- variable_meta(variable, units)

      if (nrow(hourly) == 0 || !info$obs_col %in% names(hourly)) {
        return(plotly_empty_message("No NOAA observations available"))
      }

      plot_df <- hourly |>
        transmute(
          timestamp = hour,
          value = convert_variable_values(.data[[info$obs_col]], variable, units)
        ) |>
        filter(!is.na(value)) |>
        arrange(timestamp)

      plot_noaa_timeseries_plotly(plot_df,
        title = paste0("NOAA ", info$label, " (Hourly)"),
        unit_label = info$unit
      )
    })

    output$noaa_source_mix_plot <- renderPlotly({
      raw <- noaa_tab_obs_raw()
      if (nrow(raw) == 0 || !"data_source" %in% names(raw)) {
        return(plotly_empty_message("No NOAA source metadata available"))
      }

      source_df <- raw |>
        mutate(source = ifelse(is.na(data_source) | data_source == "", "Unknown", data_source)) |>
        count(source, name = "n") |>
        arrange(desc(n))

      plot_noaa_source_mix_plotly(source_df)
    })

    output$noaa_vs_tempest_plot <- renderPlotly({
      variable <- input$noaa_variable %||% "temperature"
      units <- input$units_system %||% "metric"
      info <- variable_meta(variable, units)

      comp <- build_obs_comparison_hourly(
        noaa_tab_obs_hourly(),
        unified_tempest_hourly(),
        variable_name = variable
      )

      if (nrow(comp) == 0) {
        return(plotly_empty_message("No overlap between NOAA and Tempest for selected range"))
      }

      comp <- comp |>
        mutate(
          noaa_value = convert_variable_values(noaa_value, variable, units),
          tempest_value = convert_variable_values(tempest_value, variable, units),
          delta_noaa_minus_tempest = noaa_value - tempest_value
        )

      plot_unified_obs_delta_plotly(comp,
        variable_label = info$label,
        unit_label = info$unit
      )
    })

    latest_noaa_forecast_run <- reactive({
      data <- forecast_tab_hourly()
      if (nrow(data) == 0) {
        return(tibble())
      }

      latest_run <- max(data$generated_hour, na.rm = TRUE)
      data |>
        filter(generated_hour == latest_run) |>
        arrange(forecast_hour)
    })

    latest_openmeteo_forecast_run <- reactive({
      data <- openmeteo_forecast_hourly()
      if (nrow(data) == 0) {
        return(tibble())
      }

      latest_run <- max(data$generated_hour, na.rm = TRUE)
      data |>
        filter(generated_hour == latest_run) |>
        arrange(forecast_hour)
    })

    latest_selected_forecast <- reactive({
      provider <- input$forecast_provider %||% "blend"
      noaa <- latest_noaa_forecast_run()
      openmeteo <- latest_openmeteo_forecast_run()

      if (identical(provider, "noaa")) {
        return(noaa)
      }
      if (identical(provider, "openmeteo")) {
        return(openmeteo)
      }

      if (nrow(noaa) == 0 && nrow(openmeteo) == 0) {
        return(tibble())
      }
      if (nrow(noaa) == 0) {
        return(openmeteo)
      }
      if (nrow(openmeteo) == 0) {
        return(noaa)
      }

      noaa |>
        select(forecast_hour, starts_with("noaa_fcst_")) |>
        full_join(
          openmeteo |>
            select(forecast_hour, starts_with("om_fcst_")),
          by = "forecast_hour"
        ) |>
        mutate(
          generated_hour = floor_date(Sys.time(), unit = "hour"),
          lead_hours = as.numeric(difftime(forecast_hour, generated_hour, units = "hours")),
          noaa_fcst_temperature_c = coalesce((noaa_fcst_temperature_c + om_fcst_temperature_c) / 2, noaa_fcst_temperature_c, om_fcst_temperature_c),
          noaa_fcst_humidity = coalesce((noaa_fcst_humidity + om_fcst_humidity) / 2, noaa_fcst_humidity, om_fcst_humidity),
          noaa_fcst_pressure_hpa = coalesce((noaa_fcst_pressure_hpa + om_fcst_pressure_hpa) / 2, noaa_fcst_pressure_hpa, om_fcst_pressure_hpa),
          noaa_fcst_wind_speed = coalesce((noaa_fcst_wind_speed + om_fcst_wind_speed) / 2, noaa_fcst_wind_speed, om_fcst_wind_speed),
          noaa_fcst_wind_gust = coalesce((noaa_fcst_wind_gust + om_fcst_wind_gust) / 2, noaa_fcst_wind_gust, om_fcst_wind_gust)
        ) |>
        arrange(forecast_hour)
    })

    output$forecast_summary_boxes <- renderUI({
      raw <- forecast_tab_raw()
      prepared_noaa <- forecast_tab_hourly()
      prepared_openmeteo <- openmeteo_forecast_hourly()
      latest <- latest_selected_forecast()
      provider <- input$forecast_provider %||% "blend"

      run_count <- if (nrow(prepared_noaa) > 0) dplyr::n_distinct(prepared_noaa$generated_hour) else 0
      max_lead <- if (nrow(latest) > 0) round(max(latest$lead_hours, na.rm = TRUE), 0) else NA
      latest_gen <- if (nrow(latest) > 0) max(latest$generated_hour, na.rm = TRUE) else NA
      valid_points <- if (identical(provider, "openmeteo")) nrow(prepared_openmeteo) else nrow(raw)

      tagList(
        weather_value_box(run_count, "NOAA Runs", icon = "history", color = "humidity", width = 3),
        weather_value_box(
          if (is.na(max_lead)) "--" else paste0(max_lead, "h"),
          "Selected Lead Max",
          icon = "hourglass-half",
          color = "wind",
          width = 3
        ),
        weather_value_box(
          if (is.na(latest_gen)) "--" else format(with_tz(latest_gen, timezone_display), "%m-%d %H:%M"),
          "Selected Run Time",
          icon = "clock",
          color = "pressure",
          width = 3
        ),
        weather_value_box(valid_points, paste0("", toupper(substr(provider, 1, 1)), substr(provider, 2, nchar(provider)), " Points"), icon = "project-diagram", color = "info", width = 3)
      )
    })

    output$forecast_latest_run_plot <- renderPlotly({
      variable <- input$forecast_variable %||% "temperature"
      units <- input$units_system %||% "metric"
      info <- variable_meta(variable, units)
      latest <- latest_selected_forecast()
      provider <- input$forecast_provider %||% "blend"
      fcst_col <- resolve_forecast_column(info, if (identical(provider, "openmeteo")) "openmeteo" else "noaa")

      if (nrow(latest) == 0 || !fcst_col %in% names(latest)) {
        return(plotly_empty_message("No forecast data for selected horizon"))
      }

      plot_df <- latest |>
        transmute(
          timestamp = forecast_hour,
          value = convert_variable_values(.data[[fcst_col]], variable, units)
        ) |>
        filter(!is.na(value))

      plot_forecast_latest_run_plotly(plot_df,
        variable_label = paste0(info$label, " (", tools::toTitleCase(provider), ")"),
        unit_label = info$unit
      )
    })

    output$forecast_spread_plot <- renderPlotly({
      variable <- input$forecast_variable %||% "temperature"
      units <- input$units_system %||% "metric"
      info <- variable_meta(variable, units)
      data <- forecast_tab_hourly()

      if (nrow(data) == 0 || !info$fcst_col %in% names(data)) {
        return(plotly_empty_message("No forecast spread data available"))
      }

      spread_df <- data |>
        transmute(forecast_hour, value = .data[[info$fcst_col]]) |>
        filter(!is.na(value)) |>
        group_by(forecast_hour) |>
        summarise(
          p10 = quantile(value, probs = 0.1, na.rm = TRUE),
          p50 = quantile(value, probs = 0.5, na.rm = TRUE),
          p90 = quantile(value, probs = 0.9, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) |>
        mutate(
          p10 = convert_variable_values(p10, variable, units),
          p50 = convert_variable_values(p50, variable, units),
          p90 = convert_variable_values(p90, variable, units)
        )

      plot_forecast_spread_plotly(spread_df,
        variable_label = info$label,
        unit_label = info$unit
      )
    })

    output$forecast_provider_compare_plot <- renderPlotly({
      variable <- input$forecast_variable %||% "temperature"
      units <- input$units_system %||% "metric"
      info <- variable_meta(variable, units)

      noaa <- latest_noaa_forecast_run()
      openmeteo <- latest_openmeteo_forecast_run()

      noaa_col <- info$fcst_col
      om_col <- info$fcst_col_openmeteo

      compare_df <- full_join(
        noaa |>
          transmute(forecast_hour, noaa_value = .data[[noaa_col]]),
        openmeteo |>
          transmute(forecast_hour, openmeteo_value = .data[[om_col]]),
        by = "forecast_hour"
      ) |>
        mutate(
          noaa_value = convert_variable_values(noaa_value, variable, units),
          openmeteo_value = convert_variable_values(openmeteo_value, variable, units)
        ) |>
        filter(!is.na(noaa_value) | !is.na(openmeteo_value)) |>
        arrange(forecast_hour)

      plot_forecast_provider_compare_plotly(compare_df,
        variable_label = info$label,
        unit_label = info$unit
      )
    })

    output$forecast_precip_probability_plot <- renderPlotly({
      provider <- input$forecast_provider %||% "blend"

      if (identical(provider, "openmeteo")) {
        raw <- openmeteo_forecast_raw()
        if (nrow(raw) == 0 || !"precipitation_probability" %in% names(raw)) {
          return(plotly_empty_message("No precipitation probability data available"))
        }

        plot_df <- raw |>
          mutate(probability = suppressWarnings(as.numeric(precipitation_probability))) |>
          transmute(timestamp = forecast_time, probability) |>
          filter(!is.na(probability)) |>
          arrange(timestamp)
      } else {
        raw <- forecast_tab_raw()
        if (nrow(raw) == 0 || !all(c("generated_at", "forecast_time", "precipitation_probability") %in% names(raw))) {
          return(plotly_empty_message("No precipitation probability data available"))
        }

        latest_run <- max(raw$generated_at, na.rm = TRUE)
        plot_df <- raw |>
          filter(generated_at == latest_run) |>
          mutate(
            probability = suppressWarnings(as.numeric(precipitation_probability)),
            probability = ifelse(probability <= 1, probability * 100, probability)
          ) |>
          transmute(timestamp = forecast_time, probability) |>
          filter(!is.na(probability)) |>
          arrange(timestamp)
      }

      plot_forecast_probability_plotly(plot_df)
    })

    output$forecast_radar_map <- renderUI({
      query <- input$forecast_map_location %||% forecast_map_defaults$location_query
      resolved <- safe_fetch(resolve_us_location, query)

      if (nrow(resolved) == 0) {
        resolved <- tibble(
          label = forecast_map_defaults$location_query,
          latitude = 35.7796,
          longitude = -78.6382
        )
      }

      lat <- as.numeric(resolved$latitude[1])
      lon <- as.numeric(resolved$longitude[1])
      layer <- input$forecast_radar_layer %||% "radar"
      zoom <- 7

      overlay <- switch(layer,
        radar = "radar",
        clouds = "clouds",
        satellite = "satellite",
        "radar"
      )

      windy_url <- paste0(
        "https://embed.windy.com/embed2.html",
        "?lat=", round(lat, 4),
        "&lon=", round(lon, 4),
        "&detailLat=", round(lat, 4),
        "&detailLon=", round(lon, 4),
        "&width=1200",
        "&height=420",
        "&zoom=", zoom,
        "&level=surface",
        "&overlay=", overlay,
        "&menu=&message=&marker=&calendar=now",
        "&pressure=&type=map",
        "&location=coordinates",
        "&detail=&metricWind=default",
        "&metricTemp=%C2%B0C",
        "&radarRange=-1"
      )

      tags$iframe(
        src = windy_url,
        style = "width: 100%; height: 100%; border: 0; border-radius: 6px;",
        loading = "lazy",
        referrerpolicy = "no-referrer-when-downgrade"
      ) |>
        tags$div(style = "width: min(100%, 760px); aspect-ratio: 1 / 1; margin: 0 auto;")
    })

    output$forecast_map_location_status <- renderUI({
      query <- input$forecast_map_location %||% forecast_map_defaults$location_query
      match <- safe_fetch(resolve_us_location, query)
      layer <- input$forecast_radar_layer %||% "radar"

      if (nrow(match) == 0) {
        return(div(class = "text-warning", icon("map-marker-alt"), paste0(" Could not resolve location; using ", forecast_map_defaults$location_query, " fallback")))
      }

      div(
        class = "text-muted",
        icon("map-marker-alt"),
        paste0(
          " Map centered on: ", match$label[1],
          " (", round(match$latitude[1], 3), ", ", round(match$longitude[1], 3), ")",
          " • Layer: ", tools::toTitleCase(layer)
        )
      )
    })

    output$forecast_summary_table <- renderTable(
      {
        provider <- input$forecast_provider %||% "blend"
        raw <- forecast_tab_raw()
        om <- openmeteo_forecast_raw()

        if (identical(provider, "openmeteo")) {
          if (nrow(om) == 0 || !all(c("forecast_time", "temperature", "precipitation_probability") %in% names(om))) {
            return(NULL)
          }

          return(
            om |>
              arrange(forecast_time) |>
              transmute(
                `Valid Time` = format(with_tz(forecast_time, timezone_display), "%m-%d %H:%M"),
                `Summary` = paste0(
                  "Temp ", round(temperature, 1), "°C, PoP ",
                  ifelse(is.na(precipitation_probability), "--", round(precipitation_probability, 0)), "%"
                )
              ) |>
              head(12)
          )
        }

        if (nrow(raw) == 0 || !all(c("generated_at", "forecast_time", "weather_summary") %in% names(raw))) {
          return(NULL)
        }

        latest_run <- max(raw$generated_at, na.rm = TRUE)
        raw |>
          filter(generated_at == latest_run) |>
          transmute(
            `Valid Time` = format(with_tz(forecast_time, timezone_display), "%m-%d %H:%M"),
            `Summary` = weather_summary
          ) |>
          filter(!is.na(Summary), nzchar(Summary)) |>
          distinct() |>
          head(12)
      },
      striped = TRUE,
      bordered = FALSE,
      spacing = "xs",
      hover = TRUE
    )

    # ==========================================================================
    # UI OUTPUTS - CURRENT CONDITIONS
    # ==========================================================================

    # Current conditions panel
    output$current_conditions <- renderUI({
      req(rv$latest_obs)
      units <- input$units_system
      current_conditions_panel(
        observation = rv$latest_obs,
        alerts = rv$alerts,
        precip_24h = rv$precip_24h,
        lightning_24h = rv$lightning_24h,
        units_system = units
      )
    })

    # Alert banner
    output$alert_banner <- renderUI({
      alert_banner(rv$alerts)
    })

    # Alert dropdown menu in header
    output$alert_menu <- renderMenu({
      alerts <- rv$alerts

      if (is.null(alerts) || !alerts$has_alerts) {
        dropdownMenu(type = "notifications", badgeStatus = NULL)
      } else {
        # Create notification items from alerts
        items <- lapply(names(alerts$conditions), function(name) {
          cond <- alerts$conditions[[name]]
          if (isTRUE(cond$active)) {
            notificationItem(
              text = cond$message,
              icon = icon(if (cond$severity == "danger") "exclamation-triangle" else "exclamation-circle"),
              status = cond$severity
            )
          }
        })

        items <- Filter(Negate(is.null), items)

        dropdownMenu(
          type = "notifications",
          badgeStatus = "danger",
          .list = items
        )
      }
    })

    # Current time display
    output$current_time_display <- renderUI({
      invalidateLater(1000, session)
      # Update every second

      current_time <- with_tz(Sys.time(), timezone_display)

      p(format(current_time, "%H:%M:%S"))
    })

    # ==========================================================================
    # UI OUTPUTS - CHARTS
    # ==========================================================================

    # Temperature plot
    output$temp_plot <- renderPlotly({
      req(historical_data())
      df <- historical_data()
      if (nrow(df) == 0) {
        return(plotly_empty_message("No temperature data for selected period"))
      }
      units <- input$units_system
      temp_unit <- if (units == "us") "F" else "C"
      plot_temperature_plotly(df, show_feels_like = FALSE, unit = temp_unit)
    })

    # Humidity plot
    output$humidity_plot <- renderPlotly({
      req(historical_data())
      df <- historical_data()

      if (nrow(df) == 0) {
        return(plotly_empty_message("No humidity data for selected period"))
      }

      plot_humidity_plotly(df)
    })

    # Wind plot
    output$wind_plot <- renderPlotly({
      req(historical_data())
      df <- historical_data()
      if (nrow(df) == 0) {
        return(plotly_empty_message("No wind data for selected period"))
      }
      units <- input$units_system
      wind_unit <- if (units == "us") "mph" else "ms"
      plot_wind_plotly(df, unit = wind_unit)
    })

    # Wind rose plot
    output$wind_rose_plot <- renderPlotly({
      df <- historical_data()

      if (is.null(df) || nrow(df) == 0) {
        return(plotly_empty_message("No wind direction data"))
      }

      # Use rapid wind data if available for better resolution
      rapid <- rapid_wind_data()
      if (!is.null(rapid) && nrow(rapid) > 0) {
        df <- rapid |>
          rename(
            wind_avg = wind_speed_avg,
            wind_dir = wind_dir_avg,
            timestamp = minute_timestamp
          )
      }

      plot_wind_rose_plotly(df)
    })

    # Precipitation plot
    output$precip_plot <- renderPlotly({
      req(historical_data())
      df <- historical_data()
      if (nrow(df) == 0) {
        return(plotly_empty_message("No precipitation data for selected period"))
      }
      units <- input$units_system
      precip_unit <- if (units == "us") "in" else "mm"
      plot_precipitation_plotly(df, unit = precip_unit)
    })

    # Pressure plot
    output$pressure_plot <- renderPlotly({
      req(historical_data())
      df <- historical_data()

      if (nrow(df) == 0) {
        return(plotly_empty_message("No pressure data for selected period"))
      }

      plot_pressure_plotly(df)
    })

    # Lightning section (conditional)
    output$lightning_section <- renderUI({
      df <- historical_data()

      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      lightning_total <- sum(df$lightning_count, na.rm = TRUE)

      if (lightning_total == 0 && rv$lightning_24h == 0) {
        return(
          div(
            style = "text-align: center; padding: 20px; color: #6C757D;",
            icon("bolt", style = "font-size: 2rem; opacity: 0.5;"),
            p("No lightning activity detected in the selected period")
          )
        )
      }

      tagList(
        h5(icon("bolt"), " Lightning Activity", style = "color: #F4A261;"),
        plotlyOutput("lightning_plot", height = "200px")
      )
    })

    # Lightning plot
    output$lightning_plot <- renderPlotly({
      req(historical_data())
      df <- historical_data()
      plot_lightning_plotly(df)
    })

    # ========================================================================
    # UI OUTPUTS - UNIFIED VIEW
    # ========================================================================

    unified_var_info <- reactive({
      variable <- input$unified_variable
      units <- input$units_system
      if (is.null(variable)) variable <- "temperature"
      if (is.null(units)) units <- "metric"

      label_map <- c(
        temperature = "Temperature",
        humidity = "Humidity",
        pressure = "Pressure",
        wind_avg = "Wind (Average)",
        wind_gust = "Wind (Gust)",
        precip = "Precipitation"
      )

      unit_metric <- c(
        temperature = "°C",
        humidity = "%",
        pressure = "hPa",
        wind_avg = "m/s",
        wind_gust = "m/s",
        precip = "mm"
      )

      unit_us <- c(
        temperature = "°F",
        humidity = "%",
        pressure = "hPa",
        wind_avg = "mph",
        wind_gust = "mph",
        precip = "in"
      )

      list(
        variable = variable,
        label = label_map[[variable]],
        unit = if (units == "us") unit_us[[variable]] else unit_metric[[variable]],
        units_system = units
      )
    })

    convert_unified_values <- function(df, columns, info) {
      out <- df
      if (nrow(out) == 0) {
        return(out)
      }

      var <- info$variable
      is_us <- identical(info$units_system, "us")

      for (column_name in columns) {
        if (!column_name %in% names(out)) {
          next
        }

        if (var == "temperature" && is_us) {
          out[[column_name]] <- convert_temperature(out[[column_name]], from = "C", to = "F")
        } else if ((var == "wind_avg" || var == "wind_gust") && is_us) {
          out[[column_name]] <- convert_wind_speed(out[[column_name]], to = "mph")
        } else if (var == "precip" && is_us) {
          out[[column_name]] <- convert_precipitation(out[[column_name]], from = "mm", to = "in")
        }
      }

      out
    }

    # Helper to render unified plots with standard conversions
    render_unified_plot <- function(data_reactive, columns, plot_fn) {
      info <- unified_var_info()
      data <- data_reactive()
      if (nrow(data) == 0) {
        return(plotly_empty_message("No data available"))
      }
      data <- convert_unified_values(data, columns, info)
      plot_fn(data, variable_label = info$label, unit_label = info$unit)
    }

    output$unified_obs_delta_plot <- renderPlotly({
      render_unified_plot(
        unified_obs_comparison,
        c("noaa_value", "tempest_value", "delta_noaa_minus_tempest"),
        plot_unified_obs_delta_plotly
      )
    })

    output$unified_forecast_evolution_plot <- renderPlotly({
      info <- unified_var_info()
      data <- unified_forecast_accuracy()

      if (nrow(data) == 0) {
        return(plotly_empty_message("No forecast data available"))
      }
      if (is.null(input$unified_target_hour) || !nzchar(input$unified_target_hour)) {
        return(plotly_empty_message("Select a target hour in the sidebar"))
      }

      target_hour <- suppressWarnings(as.POSIXct(input$unified_target_hour, tz = "UTC"))
      if (is.na(target_hour)) {
        return(plotly_empty_message("Invalid target hour"))
      }

      target_data <- data |>
        filter(forecast_hour == target_hour) |>
        arrange(desc(lead_hours))

      if (nrow(target_data) == 0) {
        return(plotly_empty_message("No forecast data for this target hour"))
      }

      target_data <- convert_unified_values(target_data, c("forecast_value", "actual_value", "error", "abs_error"), info)
      plot_forecast_evolution_plotly(target_data, variable_label = info$label, unit_label = info$unit)
    })

    output$unified_accuracy_plot <- renderPlotly({
      render_unified_plot(
        unified_forecast_accuracy,
        c("forecast_value", "actual_value", "error", "abs_error"),
        plot_forecast_accuracy_plotly
      )
    })

    # ==========================================================================
    # UI OUTPUTS - STATION INFO
    # ==========================================================================

    output$station_info <- renderUI({
      station_info_panel(
        station_id = selected_station(),
        device_status = rv$device_status,
        hub_status = rv$hub_status
      )
    })

    # ==========================================================================
    # MODAL DIALOGS
    # ==========================================================================

    # Alert settings modal
    observeEvent(input$show_settings, {
      showModal(modalDialog(
        title = "Alert Threshold Settings",
        size = "m",
        p("Customize alert thresholds for weather conditions:"),
        hr(),
        fluidRow(
          column(
            width = 6,
            numericInput(
              "threshold_heavy_rain",
              "Heavy Rain (mm)",
              value = alert_thresholds$heavy_rain_mm,
              min = 0,
              step = 0.5
            ),
            numericInput(
              "threshold_high_wind",
              "High Wind (m/s)",
              value = alert_thresholds$high_wind_ms,
              min = 0,
              step = 1
            ),
            numericInput(
              "threshold_freezing",
              "Freezing Temp (°C)",
              value = alert_thresholds$freezing_temp_c,
              step = 1
            )
          ),
          column(
            width = 6,
            numericInput(
              "threshold_heat_warning",
              "Heat Warning (°C)",
              value = alert_thresholds$heat_warning_c,
              min = 20,
              step = 1
            ),
            numericInput(
              "threshold_low_battery",
              "Low Battery (V)",
              value = alert_thresholds$low_battery_v,
              min = 2.0,
              max = 3.0,
              step = 0.1
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_thresholds", "Save", class = "btn-primary")
        )
      ))
    })

    # Save threshold settings
    observeEvent(input$save_thresholds, {
      # In a production app, these would be saved to a config file or database
      showNotification(
        "Threshold settings would be saved here. (Feature placeholder)",
        type = "message"
      )
      removeModal()
    })

    # ==========================================================================
    # SESSION CLEANUP
    # ==========================================================================

    session$onSessionEnded(function() {
      # Any session-specific cleanup
      # Note: Pool is managed at app level, not closed here
    })
  }
}
