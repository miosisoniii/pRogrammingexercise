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
                   ## select variables for input FROM INPUT MODULE
                   inputmodule_UI(ns("input1")),
      ),
      
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel(ns("patientdat"), title = "Plot Data",
                             ## plot output
                             plotmodule_UI(ns("testplotoutput"))
                             ),
                    
                    tabPanel(ns("datatable"), title = "Data Table",
                             ## table output after filtering
                             outputmodule_UI(ns("datafilt_output"))
                             
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
      
      # calling for input1
      myobject <- callModule(inputmodule_Server, "input1")
      callModule(outputmodule_Server, "varselect_output", myobject)
      
      # calling plotting object, calling the same input from the input
      myobject2 <- callModule(inputmodule_Server, "input1")
      callModule(plotmodule_Server, "testplotoutput", myobject2)


    }
  )
}
