FROM rocker/r-ver:4.4.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'httr2', 'DT', 'dplyr'), repos='https://cloud.r-project.org/')"

WORKDIR /app
COPY app.R /app/app.R

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=8080)"]
