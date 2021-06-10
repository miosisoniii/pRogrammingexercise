#-------------------------------------------------------------------------------------#
# Project: server.R 
# Purpose: Shiny Application app.R
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Shiny Server (Parent)
#-------------------------------------------------------------------------------------#

shinyServer(function(input, output, session) {
  
  # labServer("main")
  
  patientServer("main")
  
})



