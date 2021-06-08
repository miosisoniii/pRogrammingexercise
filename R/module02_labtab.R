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

labUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             textInput(ns("name"), "Enter name here:"),
      )
    ),
    fluidRow(
      column(width = 6,
             selectizeInput(ns("data"), "Select data to view:",
                            choices = c("Patient Data", "Laboratory Results"))
      )
    )
  )
}

#-------------------------------------------------------------------------------------#
# Lab Tab Server
#-------------------------------------------------------------------------------------#

labServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
    }
  )
}

# labServer <- function(id, prefix = "") {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       output$result <- renderText({
#         paste0(prefix, toupper(input$txt))
#       })
#     }
#   )
# }
