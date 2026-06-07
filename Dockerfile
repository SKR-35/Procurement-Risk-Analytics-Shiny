FROM rocker/shiny:4.4.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'plotly', 'DT', 'dplyr', 'scales', \
  'htmltools', 'pagedown', 'arrow', 'stringr', 'tibble', 'readr', \
  'jsonlite', 'lubridate', 'grid', 'httr' \
), repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server/procurement-risk-analytics

COPY app.R .
COPY R/ R/
COPY outputs/ outputs/
COPY README.md .
COPY LICENSE .

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/procurement-risk-analytics', host='0.0.0.0', port=3838)"]