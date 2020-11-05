FROM rocker/r-ver:3.6.3
MAINTAINER Thomas DENECKER (thomas.denecker@gmail.com)

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \ 
    gnupg2 \
    libxml2-dev \
    libssl-dev \
    libpq-dev \
    libv8-dev \
    default-jre \
    r-cran-rjava \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN apt-get update  
RUN apt-get install -y default-jdk
RUN apt-get install -y r-cran-rjava
RUN R CMD javareconf

## Install packages for shiny
RUN Rscript -e "install.packages(installed.packages()[,'Package'])"
RUN Rscript -e "install.packages(c('shiny','shinyjs', 'shinyalert','shinycustomloader', 'shinydashboard', 'shinydashboardPlus', 'shinyFiles', 'shinyWidgets', 'shinycssloaders' ,'shinyhelper' , 'colourpicker','shinytest', 'packrat', 'testthat'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages for Visualisation

RUN Rscript -e "install.packages(c('DT', 'plotly', 'ggiraph', 'UpSetR'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages for treatement
RUN Rscript -e "install.packages(c('dplyr','reshape2'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install phantomjs
RUN Rscript -e "webdriver::install_phantomjs()"

## Install packages for various treatements
RUN Rscript -e "install.packages(c('ape', 'knitr', 'rlist', 'rJava', 'seqinr', 'svglite', 'V8', 'xlsx'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) {install.packages('BiocManager')} ; BiocManager::install(c('Biostrings','DECIPHER'), ask=F)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install devtools
RUN Rscript -e "install.packages('devtools', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages PIPprofileR
RUN Rscript -e "library(devtools) ; install_github('IFB-ElixirFr/PIPprofileR')" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

EXPOSE 3838
CMD ["R", "-e", "PIPprofileR::shiny_application(port = 3838, host = '0.0.0.0')"]
