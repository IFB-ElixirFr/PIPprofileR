################################################################################
# Title : Monitoring with sar, docker and PIPprofileR
# Maintener : Thomas Denecker
# Email : thomas.denecker@gmail.com
# Date : nov, 2020
# GitHub : https://github.com/IFB-ElixirFr/PIPprofileR
# DockerHub : https://hub.docker.com/repository/docker/tdenecker/pip-profiler
################################################################################

################################################################################
###                                Library                                   ###
################################################################################
library(ggplot2)

################################################################################
# BASH
################################################################################

### Generate monitoring file
# sar -P ALL > monitoring.txt

### Remove space and create a tsv file
# unexpand -a monitoring.txt > monitoring.tsv

### Run Script R in docker piprofiler
# docker run -it -v ${PWD}:/home/ tdenecker/pip-profiler bash -c "Rscript /home/monitoring.R"


################################################################################
# R script
################################################################################

# Change directory
setwd("/home/")

#===============================================================================
# Get date
#===============================================================================

date <- readLines("monitoring.tsv", n = 1)
date <- unlist(strsplit(date, "\t"))[2]
date <- gsub("/", "-", date)

#===============================================================================
# Prepare data
#===============================================================================
data <- read.csv2("monitoring.tsv", sep="\t", skip = 4, row.names = NULL)
colnames(data)[1:3] = c("Times", "CPU_name", "Percent")
data <- data[-grep("CPU", data$CPU), ]
data <- data[, 1:3]
data$Percent = as.numeric(as.character(data$Percent))
data$Times = factor(data$Times , levels = unique(data$Times ))
data$CPU_name = factor(data$CPU_name , levels = unique(data$CPU_name ))
average = subset(data, Times == 'Average:' )
data <- subset(data, Times != 'Average:' )

data_all <- subset(data, CPU_name == 'all' )
data <- subset(data,CPU_name != 'all' )

#===============================================================================
# Plot
#===============================================================================

#-------------------------------------------------------------------------------
# Profile
#-------------------------------------------------------------------------------

firstInHour = na.omit(unlist(lapply(0:23,function(x){
  if(x < 10){
    grep(paste0("^0",x,":"), data$Times)[1]
  } else
  {
    grep(paste0("^",x,":"), data$Times)[1]
  }
})))

p <- ggplot(data, aes(x = Times, y = Percent,
                      group = CPU_name, color = CPU_name)) +
  labs(title=paste("Monitoring -", date),
       x ="Times",
       color = "CPU") +
  geom_line() + theme_classic() +
  scale_x_discrete(breaks = as.character(data$Times[firstInHour]),
                   labels = as.character(data$Times[firstInHour])) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(paste0("monitoring_profile_",date,".png"), p,
       units="in", width=15, height=5, dpi=300)

#-------------------------------------------------------------------------------
# Barplot
#-------------------------------------------------------------------------------

p <- ggplot(data, aes(x = Times, y = Percent)) +
  geom_col(aes(fill = CPU_name), width = 0.7)+ theme_classic() +
  labs(title=paste("Monitoring -", date),
       x ="Times",
       fill = "CPU") +
  scale_x_discrete(breaks = as.character(data$Times[firstInHour]),
                   labels = as.character(data$Times[firstInHour])) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(paste0("monitoring_barplot_",date,".png"), p,
       units="in", width=15, height=5, dpi=300)
