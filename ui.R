#-------------------------------------------------------------------------------------#
# Project: ui.R
# Purpose: Shiny application UI
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

shinyUI(fluidPage(
  labUI("main")
))



### original example code from R
# shinyUI(fluidPage(
#   
#   titlePanel("Old Faithful Geyser Data"),
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
#     ),
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   )
#   
# ))