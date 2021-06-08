#-------------------------------------------------------------------------------------#
# Project: Global.R
# Purpose: Load all common packages and data
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Load Dependencies
#-------------------------------------------------------------------------------------#
require(dplyr)
require(reshape2)
require(shiny)
require(data.table)
require(ggplot2)

## source all files in the R folder
lapply(list.files("./R"), FUN = function(x) source(paste0("./R/", x)))
