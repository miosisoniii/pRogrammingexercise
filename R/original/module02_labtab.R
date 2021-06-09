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

plotdata_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 8,
            # plotOutput(ns("filtered")),
            ## test return of selected variables from patientServer tab
            verbatimTextOutput(ns("testoutput2"))
            
            
            
            ## export plot to UI
            # plotOutput(ns("filteredplot"))
             
      )
    )
  )
}

## attempt to create function that will be called
# testmodule <- function(input, output, session) (
#   callModule(patientServer, "")
# )

#-------------------------------------------------------------------------------------#
# Lab Tab Server
#-------------------------------------------------------------------------------------#

plotdata_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      # code that creates a reactive output
      ## attempt to return a single input - this works reactively!
      # testinputselect <- reactive({input$race})
      testinputselect <- reactive({
        racetest <- input$race
        sextest <- input$sex
        
        ## make inputs into list to be passed out of reactive expression
        testlist <- list(racetest, sextest)
        return(testlist)
      })
      ### testing reactive display passing from module to module
      output$testoutput2 <- renderPrint({
        testinputselect()
      })
      
      
      
      # ## rename variables for filtering of the plot
      # filtered_dat <- reactive({
      #   filtarm <- input$arm
      #   filtrace <- input$race
      #   filttest <- input$test
      #   filtcategory <- input$category
      #   filtsex <- input$sex
      #   filtage <- input$age
      #   
      #   ## filter using the reactive inputs
      #   filter_var <- melt_dat %>%
      #     filter(ACTARM == filtarm,
      #            RACE == filtrace,
      #            LBTEST == filttest,
      #            LBCAT == filtcategory,
      #            SEX == filtsex,
      #            AGE == filtage)
      #   
      #   return(filter_var)
      #     
      # })
      
      
      
      
      ## send plot output to the UI
      output$filteredplot <- renderPlot(
        ggplot(data = filtered_dat(), 
               aes(x = VISIT, y = RESULT, color = ACTARM)) + 
          geom_point()
      )
      
    }
  )
}

