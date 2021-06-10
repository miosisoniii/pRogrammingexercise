#-------------------------------------------------------------------------------------#
# Project: Module02: Lab Breakdown Tab
# Purpose: 
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
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
             verbatimTextOutput(ns("testoutput3")),
             dataTableOutput(ns("testdata")),
             plotOutput(ns("plot1")) 
             
      )
    )
  )
}

#-------------------------------------------------------------------------------------#
# Lab Tab Server
#-------------------------------------------------------------------------------------#

## good reference for variety of ways to pass objects between modules
#https://stackoverflow.com/questions/46555355/passing-data-within-shiny-modules-from-module-1-to-module-2

plotlabServer <- function(input, output, session) {
  # testing reactive to see if anything from the tab module is transferrred
  pulldata <- reactive({
    data <- patientServer("lab_output")
    return(data)
  })


  # testing datatable view from module01
  output$testdata <- renderDataTable(
    pulldata()

  )
  
  
  ## testing the print and this shit works!
  output$testoutput2 <- renderPrint("Testing to see if this text appears")
  # output$testoutput2 <- renderPrint(pulldata()$datafilt())
  
  ## testing an additional output
  ## This DOES NOT WORK because this is the second object being passed from this module
  output$testoutput3 <- renderPrint("This is the second input")
}





