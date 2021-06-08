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
        radioButtons(ns("radiotest"), "Select Arm",
                     c("ARM A" = "A",
                       "ARM B" = "B",
                       "ARM C" = "C"),
                     selected = "ARM B"),
        checkboxGroupInput(ns("race"), "Patient Race: ", 
                           choices = unique(cast_dat$RACE),
                           # selected = unique(cast_dat$RACE)),
                           selected = "WHITE"),
        checkboxGroupInput(ns("test"), "Laboratory Test: ",
                              choices = unique(cast_dat$LBTEST),
                           selected = 1
                              # selected = unique(cast_dat$LBTEST)
                           ),
        checkboxGroupInput(ns("category"), "Test Category",
                              choices = unique(cast_dat$LBCAT),
                              selected = unique(cast_dat$LBCAT)
                           )
      ),
      
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel(ns("patientdat"), "Plot Data"),
                    
                    ## plot test here
                    
                    ## plot  patient demographic composition here
                    
                    ## gonna put the datatable output here
                    tabPanel(ns("datatable"), "Data Table",
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
        labdat
      })
      
    }
  )
}
