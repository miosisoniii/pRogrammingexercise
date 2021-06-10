#-------------------------------------------------------------------------------------#
# Project: Module01: Patient Breakdown Tab
# Purpose: Create module for patient data
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# Relevant columns for Patient Data
## USUBJID - patient unique identifier
## AGE - patient age
## SEX - patient sex
## RACE - patient race
## ACTARM - Descriptor for type of dose received
## ACTARMCD - Study Arm
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# Patient Tab UI
#-------------------------------------------------------------------------------------#

patientUI <- function(id) {
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
                             
                             # testing variable output here - this works!
                             # this works within the plottab UI
                             verbatimTextOutput(ns("varselect_output")),
                             
                             ## attempt to call UI from another function - this works!
                             plotlabUI(ns("testoutput2")),
                             ## attempt 
                             plotlabUI(ns("testdata"))
                             ## plot UI from module 02
                             # plotlabUI(ns("filteredplot"))
                             
                             
                             ## plot test here
                             
                             ## plot  patient demographic composition here
                             
                             ),
                    ## table output after filtering
                    tabPanel(ns("datatable"), title = "Data Table",
                             # render data output for lab regardless of selection
                             dataTableOutput(ns("lab_output"))
                    )
        )
      )
    )
  )
}

#-------------------------------------------------------------------------------------#
# Patient Tab Server
#-------------------------------------------------------------------------------------#

patientServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ## make the selected variables from the UI reactive
      varSelect <- reactive({
        armfilt <- input$arm
        racefilt <- input$race
        testfilt <- input$filt
        categoryfilt <- input$category
        sexfilt <- input$sex
        agefilt1 <- input$age[1]
        agefilt2 <- input$age[2]
        varlist <- list(armfilt, racefilt, testfilt, categoryfilt, sexfilt, agefilt1, agefilt2)
        # assign names to the variable list in the reactive
        names(varlist) <- c("arm", "race", "test", "category", "sex", "age1", "age2")
        return(varlist)
      })
      ## test returned output
      output$varselect_output <- renderPrint(varSelect())
      
      # filter the lab data using the reactive above
      datafilt <- reactive({
        data_out <- melt_dat %>%
          filter(SEX %in% varSelect()$sex,
                 RACE %in% varSelect()$race,
                 # AGE > varSelect()$age[1], AGE < varSelect()$age[2],
                 # AGE > varSelect()$age1, AGE < varSelect()$age2,
                 # AGE %in% seq(varSelect()$age1, varSelect()$age2),
                 ACTARM %in% varSelect()$arm,
                 LBTEST %in% varSelect()$test,
                 LBCAT %in% varSelect()$category
                 )
      })
      ## render datatable here
      output$lab_output <- renderDataTable({
        datafilt()
      })
      
      
      # testing to see if callmodule will work here - this works!
      callModule(plotlabServer, "testoutput2")
      
      #-------------------------------------------------------------------------------------#
      #  STOP HERE:  encountering error: data must be two dimensional - data frame or matrix
      #-------------------------------------------------------------------------------------#

      # call test data from other module
      callModule(plotlabServer, "testdata")
      
      # now test passing the data into the other module
      # callModule(plotlabServer, "plot1")
      

    }
  )
}
