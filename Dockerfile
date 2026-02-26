FROM rocker/shiny:4.4.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('httr2', 'DT', 'dplyr', 'plotly', 'jsonlite'), repos='https://cloud.r-project.org/')"

COPY app.R /app/app.R

WORKDIR /app
RUN mkdir -p /app/data && chmod 777 /app/data

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=8080)"]
