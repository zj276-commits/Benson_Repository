FROM rocker/shiny:4.4.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('httr2', 'DT', 'dplyr', 'plotly', 'jsonlite', 'quantmod', 'digest', 'shinycssloaders', 'shinyjs'), repos='https://cloud.r-project.org/')"

COPY *.R *.csv /app/
COPY R /app/R

WORKDIR /app
RUN mkdir -p /app/data && chmod 777 /app/data

EXPOSE 8080

CMD ["R", "-e", "message('ENV FINNHUB=', nchar(Sys.getenv('FINNHUB_API_KEY')), ' chars'); shiny::runApp('/app/app.R', host='0.0.0.0', port=8080)"]
