#-------------------------------------------------------------------------------------#
# Project: Global.R
# Purpose: Load all common packages and data
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Load Dependencies
#-------------------------------------------------------------------------------------#
library(dplyr)
library(reshape2)
library(shiny)

## read all files in the R folder
lapply(list.files("R"), FUN = function(x) source(paste0("R/", x)))
