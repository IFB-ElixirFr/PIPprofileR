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
    

COPY shiny-server.conf /etc/shiny-server/