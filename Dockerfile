FROM rocker/shiny-verse
MAINTAINER Thomas DENECKER (thomas.denecker@gmail.com)

## install R package dependencies (and clean up)
RUN apt-get update && apt-get install -y gnupg2 \
    libssl-dev \
    libpq-dev \
    libv8-dev \
    default-jre \
    r-cran-rjava \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages for shiny
RUN Rscript -e "install.packages(installed.packages()[,'Package'])"
RUN Rscript -e "install.packages(c('shiny','shinyjs', 'shinyalert','shinydashboard','shinydashboardPlus', 'shinyFiles', 'shinyWidgets', 'shinycssloaders' ,'shinyhelper' , 'colourpicker','shinytest', 'packrat', 'testthat'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages for Visualisation
RUN Rscript -e "install.packages(installed.packages()[,'Package'])"
RUN Rscript -e "install.packages(c('DT', 'plotly', 'UpSetR'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages for treatement
RUN Rscript -e "install.packages(installed.packages()[,'Package'])"
RUN Rscript -e "install.packages(c('dplyr','reshape2'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install phantomjs
RUN Rscript -e "webdriver::install_phantomjs()"

## Install packages for various treatements
RUN Rscript -e "install.packages(installed.packages()[,'Package'])"
RUN Rscript -e "install.packages(c('V8','seqinr', 'xlsx','Biostrings', 'knitr'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN apt-get update && apt-get install -y libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN apt-get update  
RUN apt-get install -y default-jdk
RUN apt-get install -y r-cran-rjava
RUN R CMD javareconf

RUN Rscript -e "install.packages(c('rJava', 'xlsx'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) {install.packages('BiocManager')} ; BiocManager::install('Biostrings', ask=F)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('rlist', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('ape', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('shinycustomloader', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    
RUN Rscript -e "install.packages('svglite', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) {install.packages('BiocManager')} ; BiocManager::install('DECIPHER', ask=F)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('ggiraph', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN mkdir -p /srv/shiny-server/log
RUN chmod -R 777 /srv/shiny-server/log
RUN mkdir -p /srv/shiny-server/tmp
RUN chmod -R 777 /srv/shiny-server/tmp
RUN mkdir -p /srv/shiny-server/panels/ui
RUN mkdir -p /srv/shiny-server/panels/server
RUN mkdir -p /srv/shiny-server/www/css
RUN mkdir -p /srv/shiny-server/www/img
RUN mkdir -p /srv/shiny-server/R_scripts

COPY shiny-server.conf /srv/shiny-server
COPY panels/*.R /srv/shiny-server/panels
COPY panels/ui/*.R /srv/shiny-server/panels/ui
COPY panels/server/*.R /srv/shiny-server/panels/server
COPY www/css /srv/shiny-server/www/css
COPY www/img /srv/shiny-server/www/img
COPY R_scripts/*.R /srv/shiny-server/R_scripts
COPY ui.R /srv/shiny-server
COPY server.R /srv/shiny-server


