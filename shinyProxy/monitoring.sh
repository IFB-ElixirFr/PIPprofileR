#!/bin/bash

### Write monitoring file
sar -P ALL > monitoring.txt

### Remove space and create a tsv file
unexpand -a monitoring.txt > monitoring.tsv

### Run Script R in docker piprofiler
docker run -it -v ${PWD}:/home/ tdenecker/pip-profiler bash -c "Rscript /home/monitoring.R"
