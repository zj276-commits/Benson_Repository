FROM rocker/shiny:4.4.0

RUN R -e "install.packages(c('shiny', 'httr2', 'DT', 'dplyr'), repos='https://cloud.r-project.org/')"

RUN rm -rf /srv/shiny-server/*

COPY app.R /srv/shiny-server/app.R

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=8080)"]
