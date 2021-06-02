#-------------------------------------------------------------------------------------#
# Project: Programming Exercise Data Cleanup
# Purpose: Cleanup source data
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Load Dependencies
#-------------------------------------------------------------------------------------#
require(dplyr)
require(reshape2)
require(shiny)

#-------------------------------------------------------------------------------------#
# Load data
#-------------------------------------------------------------------------------------#

## load patient level data
pt_dat <- read.delim("../pRogrammingexercise_repo/data/Random_PatientLevelInfo_2021.tsv",
                     sep = "\t")
# how many unique patients? 452 subjects
n_distinct(pt_dat$USUBJID) 


## load lab value data
lab_dat <- read.delim("../pRogrammingexercise_repo/data/Random_LabValuesInfo_2021.tsv",
                     sep = "\t")
# the only column that needs to be cleaned is the "AVISIT" column
unique(lab_dat$AVISIT)
# check to see if there are any other unique STUDY's - 1 study
unique(lab_dat$STUDYID)


