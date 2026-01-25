# ==============================================================================
# test_plots.R
# Unit tests for visualization functions
# ==============================================================================

library(testthat)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

# Source the modules to test
source("../R/data_processing.R")
source("../R/plot_weather.R")

# ==============================================================================
# HELPER: Create test data
# ==============================================================================

create_test_observations <- function(n = 100) {
  timestamps <- seq(
    from = Sys.time() - hours(24),
    to = Sys.time(),
    length.out = n
  )

  data.frame(
    timestamp = timestamps,
    station_id = "TEST-001",
    air_temp = rnorm(n, mean = 15, sd = 5),
    humidity = runif(n, min = 40, max = 90),
    pressure = rnorm(n, mean = 1013, sd = 5),
    wind_avg = abs(rnorm(n, mean = 3, sd = 2)),
    wind_gust = abs(rnorm(n, mean = 5, sd = 3)),
    wind_lull = abs(rnorm(n, mean = 1, sd = 1)),
    wind_dir = runif(n, min = 0, max = 360),
    precip = abs(rnorm(n, mean = 0, sd = 0.1)),
    lightning_count = sample(c(0, 0, 0, 0, 0, 1, 2), n, replace = TRUE),
    lightning_dist = sample(c(0, 5, 10, 15), n, replace = TRUE)
  )
}

# ==============================================================================
# TEST: Plotly empty message
# ==============================================================================

test_that("plotly_empty_message creates valid plotly object", {
  p <- plotly_empty_message("Test message")

  expect_s3_class(p, "plotly")
})

# ==============================================================================
# TEST: Temperature plot (Plotly)
# ==============================================================================

test_that("plot_temperature_plotly creates valid plotly object with data", {
  df <- create_test_observations()
  df <- add_derived_columns(df)

  p <- plot_temperature_plotly(df)

  expect_s3_class(p, "plotly")
})

test_that("plot_temperature_plotly handles empty data", {
  df <- data.frame()

  p <- plot_temperature_plotly(df)

  expect_s3_class(p, "plotly")
})

# ==============================================================================
# TEST: Humidity plot (Plotly)
# ==============================================================================

test_that("plot_humidity_plotly creates valid plotly object with data", {
  df <- create_test_observations()
  df <- add_derived_columns(df)

  p <- plot_humidity_plotly(df)

  expect_s3_class(p, "plotly")
})

# ==============================================================================
# TEST: Wind plot (Plotly)
# ==============================================================================

test_that("plot_wind_plotly creates valid plotly object with data", {
  df <- create_test_observations()

  p <- plot_wind_plotly(df, unit = "kph")

  expect_s3_class(p, "plotly")
})

test_that("plot_wind_plotly respects unit parameter", {
  df <- create_test_observations()

  p_kph <- plot_wind_plotly(df, unit = "kph")
  p_mph <- plot_wind_plotly(df, unit = "mph")
  p_ms <- plot_wind_plotly(df, unit = "ms")

  expect_s3_class(p_kph, "plotly")
  expect_s3_class(p_mph, "plotly")
  expect_s3_class(p_ms, "plotly")
})

# ==============================================================================
# TEST: Wind rose (Plotly)
# ==============================================================================

test_that("plot_wind_rose_plotly creates valid plotly object", {
  df <- create_test_observations()

  p <- plot_wind_rose_plotly(df)

  expect_s3_class(p, "plotly")
})

# ==============================================================================
# TEST: Precipitation plot (Plotly)
# ==============================================================================

test_that("plot_precipitation_plotly creates valid plotly object", {
  df <- create_test_observations()

  p <- plot_precipitation_plotly(df)

  expect_s3_class(p, "plotly")
})

# ==============================================================================
# TEST: Pressure plot (Plotly)
# ==============================================================================

test_that("plot_pressure_plotly creates valid plotly object", {
  df <- create_test_observations()

  p <- plot_pressure_plotly(df)

  expect_s3_class(p, "plotly")
})

# ==============================================================================
# TEST: Lightning plot (Plotly)
# ==============================================================================

test_that("plot_lightning_plotly creates valid plotly object with lightning data", {
  df <- create_test_observations()
  df$lightning_count <- c(1, 2, rep(0, nrow(df) - 2))

  p <- plot_lightning_plotly(df)

  expect_s3_class(p, "plotly")
})

test_that("plot_lightning_plotly handles no lightning data", {
  df <- create_test_observations()
  df$lightning_count <- 0

  p <- plot_lightning_plotly(df)

  expect_s3_class(p, "plotly")
})

# ==============================================================================
# TEST: ggplot2 theme
# ==============================================================================

test_that("theme_weather returns valid ggplot2 theme", {
  theme <- theme_weather()

  expect_s3_class(theme, "theme")
})

# ==============================================================================
# TEST: Temperature plot (ggplot2)
# ==============================================================================

test_that("plot_temperature_ggplot creates valid ggplot object", {
  df <- create_test_observations()

  p <- plot_temperature_ggplot(df)

  expect_s3_class(p, "ggplot")
})

test_that("plot_temperature_ggplot handles empty data", {
  df <- data.frame()

  p <- plot_temperature_ggplot(df)

  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# TEST: Humidity plot (ggplot2)
# ==============================================================================

test_that("plot_humidity_ggplot creates valid ggplot object", {
  df <- create_test_observations()

  p <- plot_humidity_ggplot(df)

  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# TEST: Wind plot (ggplot2)
# ==============================================================================

test_that("plot_wind_ggplot creates valid ggplot object", {
  df <- create_test_observations()

  p <- plot_wind_ggplot(df, unit = "kph")

  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# TEST: Precipitation plot (ggplot2)
# ==============================================================================

test_that("plot_precipitation_ggplot creates valid ggplot object", {
  df <- create_test_observations()

  p <- plot_precipitation_ggplot(df)

  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# RUN TESTS
# ==============================================================================

# To run these tests:
# testthat::test_file("tests/test_plots.R")

message("Plot tests completed")
