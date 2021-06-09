#-------------------------------------------------------------------------------------#
# Project: 
# Purpose: 
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# 
#-------------------------------------------------------------------------------------#


plottab_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Treatment Exploration"),
    
    sidebarLayout(
      sidebarPanel(width = 4,
                   ## select variables for data
                   checkboxGroupInput(ns("arm"), "Select Study Arm: ",
                                      choices = unique(cast_dat$ACTARM),
                                      selected = unique(cast_dat$ACTARM)
                   ),
                   checkboxGroupInput(ns("race"), "Patient Race: ", 
                                      choices = unique(cast_dat$RACE),
                                      selected = unique(cast_dat$RACE)
                   ),
                   checkboxGroupInput(ns("test"), "Laboratory Test: ",
                                      choices = unique(cast_dat$LBTEST),
                                      selected = unique(cast_dat$LBTEST)
                   ),
                   checkboxGroupInput(ns("category"), "Test Category: ",
                                      choices = unique(cast_dat$LBCAT),
                                      selected = unique(cast_dat$LBCAT)
                   ),
                   checkboxGroupInput(ns("sex"), "Sex: ",
                                      choices = c("M", "F"),
                                      selected = c("M", "F")
                   ),
                   sliderInput(ns("age"), "Age Range: ",
                               min = min(cast_dat$AGE),
                               max = max(cast_dat$AGE),
                               value = c(min(cast_dat$AGE),
                                         max(cast_dat$AGE)))
      ),
      
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel(ns("patientdat"), title = "Plot Data",
                             verbatimTextOutput(ns("testoutput"))
                             
                    ),

                    ## gonna put the datatable output here
                    tabPanel(ns("datatable"), title = "Data Table",
                             # render data output for lab regardless of selection
                             # dataTableOutput(ns("testoutput2"))
                             dataTableOutput("testoutput2")
                    )
        )
      )
    )
  )
}

#-------------------------------------------------------------------------------------#
# Server
#-------------------------------------------------------------------------------------#

plottab_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ## attempt to call this reactive data from another module
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
      output$testoutput <- renderPrint({
        testinputselect()
      })
      
      ## return filtered output
      datafilt <- reactive({
        data <- melt_dat %>% 
          filter(RACE == testinputselect()[[1]],
                 SEX == testinputselect()[[2]])
        return(data)
      })
      
      output$testoutput2 <- renderDataTable(
        datafilt()
      )

    }
  )
}