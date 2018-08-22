# TODO: Add comment
# 
# Author: ecor
###############################################################################
rm(list=ls())
library(RSMET)
library(lubridate)
source('/home/ecor/Dropbox/R-packages/RSMET/R/aggregate.R')
				
smet <- as.smet(system.file("examples/T0179.smet",package="RSMET"))

outa <- aggregate(smet)
