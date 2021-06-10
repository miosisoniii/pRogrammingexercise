#-------------------------------------------------------------------------------------#
# Project: ui.R
# Purpose: Shiny application UI (Parent)
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# Shiny application UI (Parent)
#-------------------------------------------------------------------------------------#

shinyUI(
  fluidPage(
    fluidRow(
      # column(width = 6,
      #        labUI("main")
      # ),
      column(width = 8,
             patientUI("main")
      )
    )
  )
)  

