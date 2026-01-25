# ==============================================================================
# Dockerfile for Weather Station Dashboard
# R Shiny application with renv dependency management
# ==============================================================================

FROM rocker/r-ver:4.4.3

# Set environment variables
ENV RENV_VERSION=1.0.5
ENV RENV_PATHS_LIBRARY=renv/library

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /srv/shiny-app

# Copy renv infrastructure first (for better Docker layer caching)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install renv and restore packages
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"
RUN R -e "renv::restore()"

# Copy application files
COPY app.R app.R
COPY global.R global.R
COPY config.R config.R
COPY R/ R/
COPY www/ www/

# Expose Shiny default port
EXPOSE 3838

# Set up healthcheck
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]
