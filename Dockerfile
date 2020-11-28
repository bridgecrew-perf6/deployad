# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
    
RUN R -e "install.packages(pkgs=c('shiny','shinydashboard','shinythemes','DT','caret','xgboost','reactable','ids','scales'), repos='https://cran.rstudio.com/')" 
    
# copy the app to the image
COPY /AnomalyDetection/app.R /srv/shiny-server/
COPY /AnomalyDetection/www /srv/shiny-server/www
COPY /AnomalyDetection/data /srv/shiny-server/data

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]
