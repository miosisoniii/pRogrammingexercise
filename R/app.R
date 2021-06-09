#-------------------------------------------------------------------------------------#
# Project: App.R
# Purpose: 
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# Shiny application UI (Parent)
#-------------------------------------------------------------------------------------#

ui <- fluidPage(
  fluidRow(
    # column(width = 6,
    #        labUI("main")
    # ),
    column(width = 8,
           plottab_ui("main")
    )
  )
)



#-------------------------------------------------------------------------------------#
# Shiny application Server (parent)
#-------------------------------------------------------------------------------------#

server <- function(input, output, session) (
  
  # labServer("main")
  
  ## this works by pulling from the patienttab.R module
  plottab_server("main")
  
  
  # plotlabServer("filtered")
  
)

shinyApp(ui, server)