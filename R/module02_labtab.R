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
            plotOutput(ns("filtered")) 
             
      )
    )
  )
}

#-------------------------------------------------------------------------------------#
# Lab Tab Server
#-------------------------------------------------------------------------------------#

plotlabServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ## rename variables for filtering of the plot
      filtered_dat <- reactive({
        arm <- input$arm
        race <- input$race
        test <- input$test
        category <- input$category
        sex <- input$sex
        age <- input$age
        
        ## filter using the reactive inputs
        melt_dat %>%
          filter(ACTARM == arm,
                 RACE == race,
                 LBTEST == test,
                 LBCAT == category,
                 SEX == sex,
                 AGE == age)
          
      })
      
      
      
      
      ## send plot output to the UI
      output$filtered <- renderPlot(
        ggplot(data = filtered_dat(), 
               aes(x = VISIT, y = RESULT, color = ACTARM)) + 
          geom_point()
      )
      
    }
  )
}

