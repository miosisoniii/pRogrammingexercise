#-------------------------------------------------------------------------------------#
# Project: Module02: Lab Breakdown Tab
# Purpose: 
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Relevant columns for PLab Data
## USUBJID - patient unique identifier
## AGE - patient age
## SEX - patient sex
## RACE - patient race
## ACTARM - Descriptor for type of dose received
## ACTARMCD - Study Arm
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Lab Tab UI
#-------------------------------------------------------------------------------------#

plotlabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 8,
             verbatimTextOutput(ns("testoutput2")),
             dataTableOutput(ns("testdata")),
             plotOutput(ns("plot1")) 
             
      )
    )
  )
}

#-------------------------------------------------------------------------------------#
# Lab Tab Server
#-------------------------------------------------------------------------------------#



plotlabServer <- function(input, output, session) {
  # testing reactive to see if anything from the tab module is transferrred
  pulldata <- reactive({
    data <- patientServer("lab_output")
    return(data)
  })
  # print here
  
  
  ## testing the print and this shit works!
  # output$testoutput2 <- renderPrint("Testing to see if this text appears")
  output$testoutput2 <- renderPrint(pulldata()$datafilt())
  
  

  # testing datatable view from module01
  output$testdata <- renderDataTable(
    pulldata()
    
  )
  

}





