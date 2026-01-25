# ==============================================================================
# test_data_processing.R
# Unit tests for data processing functions
# ==============================================================================

library(testthat)
library(dplyr)
library(lubridate)

# Source the module to test
source("../R/data_processing.R")

# ==============================================================================
# TEST: convert_wind_speed
# ==============================================================================

test_that("convert_wind_speed converts correctly to kph", {
  expect_equal(convert_wind_speed(10, "kph"), 36)
  expect_equal(convert_wind_speed(0, "kph"), 0)
  expect_equal(convert_wind_speed(1, "kph"), 3.6)
})

test_that("convert_wind_speed converts correctly to mph", {
  expect_equal(convert_wind_speed(10, "mph"), 22.37)
  expect_equal(convert_wind_speed(0, "mph"), 0)
})

test_that("convert_wind_speed returns m/s unchanged", {
  expect_equal(convert_wind_speed(10, "ms"), 10)
})

test_that("convert_wind_speed handles NA", {
  expect_true(is.na(convert_wind_speed(NA, "kph")))
})

# ==============================================================================
# TEST: get_wind_cardinal
# ==============================================================================

test_that("get_wind_cardinal returns correct cardinal directions", {
  expect_equal(get_wind_cardinal(0), "N")
  expect_equal(get_wind_cardinal(360), "N")
  expect_equal(get_wind_cardinal(90), "E")
  expect_equal(get_wind_cardinal(180), "S")
  expect_equal(get_wind_cardinal(270), "W")
  expect_equal(get_wind_cardinal(45), "NE")
  expect_equal(get_wind_cardinal(135), "SE")
  expect_equal(get_wind_cardinal(225), "SW")
  expect_equal(get_wind_cardinal(315), "NW")
})

test_that("get_wind_cardinal handles edge cases", {
  expect_true(is.na(get_wind_cardinal(NA)))
  expect_equal(get_wind_cardinal(720), "N")
  # 2*360 = N
})

# ==============================================================================
# TEST: calculate_dew_point
# ==============================================================================

test_that("calculate_dew_point returns reasonable values", {
  # At 20°C and 50% humidity, dew point should be around 9°C
  dew_point <- calculate_dew_point(20, 50)
  expect_true(dew_point > 8 && dew_point < 11)

  # At 100% humidity, dew point equals temperature
  expect_equal(calculate_dew_point(20, 100), 20, tolerance = 0.5)
})

test_that("calculate_dew_point handles edge cases", {
  expect_true(is.na(calculate_dew_point(NA, 50)))
  expect_true(is.na(calculate_dew_point(20, NA)))
  expect_true(is.na(calculate_dew_point(20, 0)))
  expect_true(is.na(calculate_dew_point(20, -10)))
})

# ==============================================================================
# TEST: circular_mean
# ==============================================================================

test_that("circular_mean calculates correct averages", {
  # Mean of North directions
  expect_equal(circular_mean(c(350, 10)), 0, tolerance = 1)

  # Mean of East
  expect_equal(circular_mean(c(80, 100)), 90, tolerance = 1)

  # Mean of South
  expect_equal(circular_mean(c(170, 190)), 180, tolerance = 1)
})

test_that("circular_mean handles all NA", {
  expect_true(is.na(circular_mean(c(NA, NA, NA))))
})

# ==============================================================================
# TEST: calculate_feels_like
# ==============================================================================

test_that("calculate_feels_like returns temperature when no adjustment needed", {
  # Mild conditions - no wind chill or heat index needed
  expect_equal(
    calculate_feels_like(20, 50, 2),
    20
  )
})
test_that("calculate_feels_like applies wind chill in cold windy conditions", {
  # Cold and windy should feel colder
  feels_like <- calculate_feels_like(-5, 50, 10)
  expect_true(feels_like < -5)
})

test_that("calculate_feels_like applies heat index in hot humid conditions", {
  # Hot and humid should feel hotter
  feels_like <- calculate_feels_like(32, 70, 1)
  expect_true(feels_like > 32)
})

# ==============================================================================
# TEST: check_alert_conditions
# ==============================================================================

test_that("check_alert_conditions detects freezing", {
  obs <- data.frame(
    air_temp = -2,
    wind_gust = 5,
    precip = 0,
    lightning_count = 0,
    battery = 2.6
  )

  thresholds <- list(
    freezing_temp_c = 0,
    extreme_cold_c = -20,
    heat_warning_c = 35,
    extreme_heat_c = 40,
    high_wind_ms = 10,
    extreme_wind_ms = 20,
    heavy_rain_mm = 2.5,
    extreme_rain_mm = 7.5,
    lightning_detected = 0,
    low_battery_v = 2.4
  )

  alerts <- check_alert_conditions(obs, thresholds)

  expect_true(alerts$has_alerts)
  expect_true(alerts$conditions$freezing$active)
})

test_that("check_alert_conditions detects lightning", {
  obs <- data.frame(
    air_temp = 20,
    wind_gust = 5,
    precip = 0,
    lightning_count = 3,
    battery = 2.6
  )

  thresholds <- list(
    freezing_temp_c = 0,
    extreme_cold_c = -20,
    heat_warning_c = 35,
    extreme_heat_c = 40,
    high_wind_ms = 10,
    extreme_wind_ms = 20,
    heavy_rain_mm = 2.5,
    extreme_rain_mm = 7.5,
    lightning_detected = 0,
    low_battery_v = 2.4
  )

  alerts <- check_alert_conditions(obs, thresholds)

  expect_true(alerts$has_alerts)
  expect_true(alerts$conditions$lightning$active)
  expect_equal(alerts$conditions$lightning$severity, "danger")
})

test_that("check_alert_conditions returns no alerts for normal conditions", {
  obs <- data.frame(
    air_temp = 20,
    wind_gust = 5,
    precip = 0,
    lightning_count = 0,
    battery = 2.6
  )

  thresholds <- list(
    freezing_temp_c = 0,
    extreme_cold_c = -20,
    heat_warning_c = 35,
    extreme_heat_c = 40,
    high_wind_ms = 10,
    extreme_wind_ms = 20,
    heavy_rain_mm = 2.5,
    extreme_rain_mm = 7.5,
    lightning_detected = 0,
    low_battery_v = 2.4
  )

  alerts <- check_alert_conditions(obs, thresholds)

  expect_false(alerts$has_alerts)
})

# ==============================================================================
# RUN TESTS
# ==============================================================================

## ...existing code...

message(
  "Data processing tests completed"
)
