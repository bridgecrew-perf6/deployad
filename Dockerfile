FROM openanalytics/r-base

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
 libxml2-dev 
    
RUN apt-get update -qyy

RUN R -e "install.packages(pkgs=c('shiny','shinydashboard','shinythemes','DT','caret','xgboost','ids','scales'), repos='https://cran.rstudio.com/')" 


# copy the app to the image
RUN mkdir /root/AnomalyDetection
COPY AnomalyDetection /root/AnomalyDetection
COPY Rprofile.site /usr/lib/R/etc/

# select port
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app/Shiny')"]
