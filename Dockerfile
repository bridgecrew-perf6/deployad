FROM rocker/shiny:3.5.1 
RUN apt-get -y install libcurl4-gnutls-dev libxml2-dev libssl-dev; \ rm -r /srv/shiny-server; \ mkdir -p /var/lib/shiny-server/bookmarks/shiny; \ sed -i 's/3838/3838 0.0.0.0/' /etc/shiny-server/shiny-server.conf 
RUN R -e "install.packages(pkgs=c('shiny','shinydashboard','shinythemes','DT','caret','xgboost','reactable','ids','scales'), repos='https://cran.rstudio.com/')" 
COPY /AnomalyDetection /srv/shiny-server/ 
RUN chmod -R +rx /srv/shiny-server/ 
USER shiny E
XPOSE 3838 
CMD ["/usr/bin/shiny-server.sh"]