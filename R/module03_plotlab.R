#-------------------------------------------------------------------------------------#
# Project: Module03: Plot Lab Results
# Purpose: Plot Lab Results
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Lab Plot UI
#-------------------------------------------------------------------------------------#

labUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             textInput(ns("name"), "Enter name here:"),
      )
    ),
    fluidRow(
      column(width = 3,
             selectizeInput(ns("data"), "Select data to view:",
                            choices = c("Patient Data" = "pt_dat", 
                                        "Laboratory Results" = "lab_dat")),
             
             
      )
    )
  )
}

#-------------------------------------------------------------------------------------#
# Lab Plot Server
#-------------------------------------------------------------------------------------#

labServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      # select input
      # dataSelect <- input$data
      
      # reactive for selection of variables for lab
      
      # render output datatable for lab
      # output$lab_output <- renderDataTable({
      #   head(lab_dat)
      # })
      
    }
  )
}
