FROM rocker/shiny:3.5.1
RUN apt-get update && apt-get install libcurl4-openssl-dev libv8-3.14-dev -y &&\
  mkdir -p /var/lib/shiny-server/bookmarks/shiny
# Download and install library
RUN R -e "install.packages(pkgs=c('shiny','shinydashboard','shinythemes','DT','caret','xgboost','reactable','ids','scales'), repos='https://cran.rstudio.com/')" 
COPY /AnomalyDetection /srv/shiny-server/ 
# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /srv/shiny-server/
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"] 
