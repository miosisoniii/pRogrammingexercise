#-------------------------------------------------------------------------------------#
# Project: Module01: Patient Breakdown Tab
# Purpose: Create module for patient data
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# Relevant columns for Patient Data
## USUBJID - patient unique identifier
## AGE - patient age
## SEX - patient sex
## RACE - patient race
## ACTARM - Descriptor for type of dose received
## ACTARMCD - Study Arm
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# Patient Tab UI
#-------------------------------------------------------------------------------------#

patientUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Treatment Exploration"),
    
    sidebarLayout(
      sidebarPanel(width = 4,
                   ## select variables for data
                   checkboxGroupInput(ns("arm"), "Select Study Arm: ",
                                      choices = unique(cast_dat$ACTARM),
                                      selected = unique(cast_dat$ACTARM)
                   ),
                   checkboxGroupInput(ns("race"), "Patient Race: ", 
                                      choices = unique(cast_dat$RACE),
                                      selected = unique(cast_dat$RACE)
                   ),
                   checkboxGroupInput(ns("test"), "Laboratory Test: ",
                                      choices = unique(cast_dat$LBTEST),
                                      selected = unique(cast_dat$LBTEST)
                   ),
                   checkboxGroupInput(ns("category"), "Test Category: ",
                                      choices = unique(cast_dat$LBCAT),
                                      selected = unique(cast_dat$LBCAT)
                   ),
                   checkboxGroupInput(ns("sex"), "Sex: ",
                                      choices = c("M", "F"),
                                      selected = c("M", "F")
                   ),
                   sliderInput(ns("age"), "Age Range: ",
                               min = min(cast_dat$AGE),
                               max = max(cast_dat$AGE),
                               value = c(min(cast_dat$AGE),
                                         max(cast_dat$AGE)))
      ),
      
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel(ns("patientdat"), title = "Plot Data"),
                    
                    ## plot test here
                    
                    ## plot  patient demographic composition here
                    
                    ## gonna put the datatable output here
                    tabPanel(ns("datatable"), title = "Data Table",
                             # render data output for lab regardless of selection
                             dataTableOutput(ns("lab_output"))
                    )
        )
      )
    )
  )
}

#-------------------------------------------------------------------------------------#
# Patient Tab Server
#-------------------------------------------------------------------------------------#

patientServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ## render datatable here
      ## will be filtered later
      output$lab_output <- renderDataTable({
        lab_dat
      })
      
    }
  )
}
