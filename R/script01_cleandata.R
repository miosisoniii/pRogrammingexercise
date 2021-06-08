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
# require(shiny)

#-------------------------------------------------------------------------------------#
# Load data
#-------------------------------------------------------------------------------------#

## load patient level data
pt_dat <- read.delim("../pRogrammingexercise_repo/data/Random_PatientLevelInfo_2021.tsv",
                     sep = "\t")
# how many unique patients? 452 subjects
# n_distinct(pt_dat$USUBJID) 


## load lab value data
lab_dat <- read.delim("../pRogrammingexercise_repo/data/Random_LabValuesInfo_2021.tsv",
                     sep = "\t")
# the only column that needs to be cleaned is the "AVISIT" column
# unique(lab_dat$AVISIT)
# check to see if there are any other unique STUDY's - 1 study
# unique(lab_dat$STUDYID)

#-------------------------------------------------------------------------------------#
# Combine Data
#-------------------------------------------------------------------------------------#

## join by USUBJID column
both_dat <- left_join(pt_dat, lab_dat, by = c("USUBJID", "STUDYID"))

#-------------------------------------------------------------------------------------#
# Create BASELINE SUBTRACTION
#-------------------------------------------------------------------------------------#

## reshape VISIT into wide format to perform subtraction
cast_dat <- both_dat %>%
  dcast(... ~ AVISIT, value.var = "AVAL") %>%
  ## create DELTA Column to observe change from baseline
  mutate(PEAK = pmax(SCREENING, `WEEK 1 DAY 8`, `WEEK 2 DAY 15`, 
                     `WEEK 3 DAY 22`, `WEEK 4 DAY 29`,`WEEK 5 DAY 36`),
         SCRN_DELTA = SCREENING - BASELINE,
         WK1_DELTA = `WEEK 1 DAY 8` - BASELINE,
         WK2_DELTA = `WEEK 2 DAY 15` - BASELINE,
         WK3_DELTA = `WEEK 3 DAY 22` - BASELINE,
         WK4_DELTA = `WEEK 4 DAY 29` - BASELINE,
         WK5_DELTA = `WEEK 5 DAY 36` - BASELINE,
         PEAK_DELTA = pmax(SCRN_DELTA, WK1_DELTA, WK2_DELTA, WK3_DELTA, WK4_DELTA, WK5_DELTA))

## write this final output file to csv to be used in the shiny app
write.csv(cast_dat, "./output/combined_PT_LAB_clean_dat.csv", row.names = FALSE)


