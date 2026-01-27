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
#'
#' @return A Shiny server function
#' @export
# ------------------------------------------------------------------------------
build_server <- function(pool) {
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
