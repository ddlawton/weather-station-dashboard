# Weather Station Dashboard

A R Shiny dashboard for visualizing weather station data with interactive Plotly charts and publication-quality ggplot2 graphics.

## Features

### Current Implementation
- **Current Conditions**: Real-time display of temperature, humidity, wind, precipitation, pressure, and lightning
- **Historical Trends**: Interactive time-series charts with zoom, pan, and hover functionality
- **Wind Rose**: Polar chart showing wind direction distribution
- **Alert System**: Configurable thresholds for weather alerts (freezing, high wind, heavy rain, lightning)
- **Responsive Design**: Modern, mobile-friendly interface
- **Connection Pooling**: Efficient PostgreSQL database connections

### Planned Features
- NOAA data integration
- Weather forecast display
- Unified multi-source view for data comparison

## Requirements

### R Packages
```r
# Core Shiny
shiny
shinydashboard
shinycssloaders

# Data manipulation
dplyr
tidyr
lubridate

# Database
pool
DBI
RPostgres

# Visualization
ggplot2
plotly
scales

# Testing
testthat
```

### Database
- PostgreSQL with the following tables:
  - `obs_st` - Weather observations
  - `rapid_wind_1min` - High-frequency wind data
  - `hub_status` - Hub device status
  - `device_status` - Weather station device status

## Installation

1. Clone or download this repository

2. Install required R packages:
```r
install.packages(c(
  "shiny", "shinydashboard", "shinycssloaders",
  "dplyr", "tidyr", "lubridate",
  "pool", "DBI", "RPostgres",
  "ggplot2", "plotly", "scales",
  "testthat"
))
```

3. Configure database connection in `config.R`:
```r
DB_CONFIG <- list(
  host = "your_host",
  port = 5432,
  dbname = "weatherdata",
  user = "your_user",
  password = Sys.getenv("DB_PASSWORD", "your_password")
)
```

4. (Recommended) Set database password via environment variable:
```bash
export DB_PASSWORD="your_secure_password"
```

## Running the App

### Development
```r
# From R console
shiny::runApp()

# Or specify port
shiny::runApp(port = 3838)
```

### Production (Shiny Server)
1. Place the app directory in your Shiny Server apps folder
2. Access via `http://your-server:3838/weather_station_dashboard/`

### Docker (Optional)
```dockerfile
FROM rocker/shiny:latest
COPY . /srv/shiny-server/weather_dashboard
RUN R -e "install.packages(c('shinydashboard', 'pool', 'RPostgres', 'plotly', ...))"
EXPOSE 3838
```

## Project Structure

```
weather_station_dashboard/
├── app.R                    # Main application entry point
├── global.R                 # Global settings and package loading
├── config.R                 # Configuration (DB, thresholds, colors)
├── R/
│   ├── db_access.R          # Database access layer
│   ├── data_processing.R    # Data transformation functions
│   ├── plot_weather.R       # Plotly and ggplot2 visualizations
│   ├── ui_components.R      # Reusable UI components
│   ├── ui_main.R            # Main UI definition
│   └── server_logic.R       # Server reactive logic
├── tests/
│   ├── test_data_processing.R
│   └── test_plots.R
├── www/
│   └── custom.css           # Additional custom styles
└── README.md
```

## Configuration

### Alert Thresholds (config.R)
```r
alert_thresholds <- list(
  heavy_rain_mm = 2.5,       # Heavy rain warning
  extreme_rain_mm = 7.5,     # Extreme rain alert
  high_wind_ms = 10.0,       # High wind warning (m/s)
  extreme_wind_ms = 20.0,    # Extreme wind alert
  freezing_temp_c = 0,       # Freezing temperature
  heat_warning_c = 35,       # Heat warning
  low_battery_v = 2.4        # Low battery alert
)
```

### Time Windows
Available time ranges: 24 hours, 7 days, 30 days, 3 months

### Refresh Intervals
- Current conditions: Every 1 minute
- Historical data: User-selectable (1, 5, 15 minutes or off)

## Color Palette

The dashboard uses a carefully curated color palette for readability and aesthetics:

| Variable    | Color   | Hex       |
|-------------|---------|-----------|
| Temperature | Coral   | `#E63946` |
| Humidity    | Steel   | `#457B9D` |
| Rainfall    | Navy    | `#1D3557` |
| Wind        | Teal    | `#2A9D8F` |
| Lightning   | Amber   | `#F4A261` |
| Pressure    | Gray    | `#6C757D` |

## Testing

Run unit tests:
```r
testthat::test_dir("tests")
```

## Style Guide

This project follows the [Google R Style Guide](https://google.github.io/styleguide/Rguide.html):
- `snake_case` for function and variable names
- Roxygen-style documentation for functions
- Maximum 80-character line length
- Explicit function arguments

## Troubleshooting

### Database Connection Issues
1. Verify PostgreSQL is running and accessible
2. Check network connectivity to database host
3. Verify credentials in `config.R`
4. Check PostgreSQL logs for connection errors

### Missing Data
- Verify weather station is online and sending data
- Check timestamp range in database
- Review `obs_st` table for recent entries

### Performance
- For large datasets, data is automatically aggregated
- Adjust `AGGREGATION$aggregate_threshold_hours` in config
- Consider database indexing on `timestamp` columns

## License

MIT License - See LICENSE file for details

## Support

For issues and feature requests, please use the GitHub issue tracker.
