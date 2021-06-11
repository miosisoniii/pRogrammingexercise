#-------------------------------------------------------------------------------------#
# Project: Module02: Lab Breakdown Tab
# Purpose: 
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# DATATABLE INPUT MODULE
#-------------------------------------------------------------------------------------#

inputmodule_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 8,
             checkboxGroupInput(ns("arm"), "Select Study Arm: ",
                                choices = unique(cast_dat$ACTARM),
                                selected = unique(cast_dat$ACTARM)
             ),
             checkboxGroupInput(ns("race"), "Patient Race: ",
                                choices = unique(cast_dat$RACE),
                                selected = unique(cast_dat$RACE)
             ),
             checkboxGroupInput(ns("labtest"), "Laboratory Test: ",
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
      )
    )
  )
}

inputmodule_Server <- function(input, output, session) {
  
  # create reactive so that i can plot something maybe
  # BUT first I am going to try storing a reactive variable output inside the list output 
  inputselection <- reactive({
    armfilt <- input$arm
    racefilt <- input$race
    labtestfilt <- input$labtest
    categoryfilt <- input$category
    sexfilt <- input$sex
    agefilt1 <- input$age[1]
    agefilt2 <- input$age[2]
    varlist <- list(armfilt, racefilt, labtestfilt, categoryfilt, sexfilt, agefilt1, agefilt2)
    # assign names to the variable list in the reactive
    names(varlist) <- c("arm", "race", "labtest", "category", "sex", "age1", "age2")
    return(varlist)
  })
  
  ## filtered dataframe
  datafiltered <- reactive({
    data_out <- melt_dat %>%
      filter(SEX %in% input$sex,
             RACE %in% input$race,
             AGE %in% seq(input$age[1], input$age[2]),
             ACTARM %in% input$arm,
             LBTEST %in% input$labtest,
             LBCAT %in% input$category)
    return(data_out)
  })
    
  # testing the output of calling the same reactive object within this server module
  # plot_output <- ggplot2(data = datafiltered,
  #                        aes(x = ACTARM, y = RESULT)) +
  #   geom_bar()
  
  ## return reactive inputs as a list
  list(
    inputselection, # reactive input selection list format
    datafiltered # reactive dataframe output
    # plot_output # plot from filtered selection
  )
}

#-------------------------------------------------------------------------------------#
# DATATABLE OUTPUT MODULE
#-------------------------------------------------------------------------------------#

outputmodule_UI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(NS(id, "datafilt_output"))
  )
}

outputmodule_Server <- function(input, output, session, object) {
  ## test returned output
  output$varselect_output <- renderPrint(object$inputselection())
  output$datafilt_output <- renderDataTable(object$datafiltered())
}

#-------------------------------------------------------------------------------------#
# PLOT MODULE
#-------------------------------------------------------------------------------------#

plotmodule_UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(NS(id, "labplotoutput")),
    plotOutput(NS(id, "patientplotoutput")),
    # dataTableOutput(NS(id, "testplotoutput")), # WORKS! testing the output from calling the reactive object within the module above
    verbatimTextOutput(NS(id, "testtextoutput")),
    dataTableOutput(NS(id, "testtableoutput"))
  )
}

plotmodule_Server <- function(input, output, session, object) {

  ## renders the plot output data
  output$labplotoutput <- renderPlot(    # plotting output
    ggplot(data = object[[2]](),
            aes(x = BASELINE, y = RESULT)) +
      geom_point()
    )
  output$patientplotoutput <- renderPlot(    # plotting output
    ggplot(data = object[[2]](),
           aes(x = ACTARM, y = RESULT)) +
      geom_point()
  )
  
  
  
  ## output code that works
  # output$testplotoutput <- renderDataTable(object[[3]]()) # this works! using a reactive object does indeed print
  output$testtextoutput <- renderPrint(object[[1]]()) # selection output in text format
  output$testtableoutput <- renderDataTable(object[[2]]()) # selection filtered datatable output
  
}









#-------------------------------------------------------------------------------------#
# Lab Tab UI
#-------------------------------------------------------------------------------------#

# plotlabUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fluidRow(
#       column(width = 8,
#              # verbatimTextOutput(ns("testoutput2")),
#              # verbatimTextOutput(ns("testoutput3")),
#              # dataTableOutput(ns("testdata")),
#              # plotOutput(ns("plot1")),
#              ## select variables for data
#              checkboxGroupInput(ns("arm"), "Select Study Arm: ",
#                                 choices = unique(cast_dat$ACTARM),
#                                 selected = unique(cast_dat$ACTARM)
#              ),
#              checkboxGroupInput(ns("race"), "Patient Race: ",
#                                 choices = unique(cast_dat$RACE),
#                                 selected = unique(cast_dat$RACE)
#              ),
#              checkboxGroupInput(ns("labtest"), "Laboratory Test: ",
#                                 choices = unique(cast_dat$LBTEST),
#                                 selected = unique(cast_dat$LBTEST)
#              ),
#              checkboxGroupInput(ns("category"), "Test Category: ",
#                                 choices = unique(cast_dat$LBCAT),
#                                 selected = unique(cast_dat$LBCAT)
#              ),
#              checkboxGroupInput(ns("sex"), "Sex: ",
#                                 choices = c("M", "F"),
#                                 selected = c("M", "F")
#              ),
#              sliderInput(ns("age"), "Age Range: ",
#                          min = min(cast_dat$AGE),
#                          max = max(cast_dat$AGE),
#                          value = c(min(cast_dat$AGE),
#                                    max(cast_dat$AGE))),
#              dataTableOutput(ns("testdata"))
#              
#       )
#     )
#   )
# }

#-------------------------------------------------------------------------------------#
# Lab Tab Server
#-------------------------------------------------------------------------------------#

## good reference for variety of ways to pass objects between modules
#https://stackoverflow.com/questions/46555355/passing-data-within-shiny-modules-from-module-1-to-module-2

# plotlabServer <- function(input, output, session, object) {
#   ## reactive inputs
#   ## make the selected variables from the UI reactive
#   varSelect <- reactive({
#     armfilt <- input$arm
#     racefilt <- input$race
#     labtestfilt <- input$labtest
#     categoryfilt <- input$category
#     sexfilt <- input$sex
#     agefilt1 <- input$age[1]
#     agefilt2 <- input$age[2]
#     varlist <- list(armfilt, racefilt, labtestfilt, categoryfilt, sexfilt, agefilt1, agefilt2)
#     # assign names to the variable list in the reactive
#     names(varlist) <- c("arm", "race", "labtest", "category", "sex", "age1", "age2")
#     return(varlist)
#   })
#   ## test returned output
#   output$varselect_output <- renderPrint(varSelect())
#   
#   # filter the lab data using the reactive above
#   datafilt <- reactive({
#     data_out <- melt_dat %>%
#       filter(SEX %in% varSelect()$sex,
#              RACE %in% varSelect()$race,
#              AGE %in% seq(varSelect()$age1, varSelect()$age2),
#              ACTARM %in% varSelect()$arm,
#              LBTEST %in% varSelect()$labtest,
#              LBCAT %in% varSelect()$category
#       )
#   })
#   ## render datatable here
#   output$lab_output <- renderDataTable({
#     datafilt()
#   })
#   
#   
#   
#   
#   # testing reactive to see if anything from the tab module is transferred
#   pulldata <- reactive({
#     data <- patientServer("lab_output")
#     return(data)
#   })
# 
# 
#   # testing datatable view from module01
#   output$testdata <- renderDataTable(
#     pulldata()
# 
#   )
#   
#   
#   ## testing the print and this shit works!
#   output$testoutput2 <- renderPrint("Testing to see if this text appears")
#   # output$testoutput2 <- renderPrint(pulldata()$datafilt())
#   
#   ## testing an additional output
#   ## This DOES NOT WORK because this is the second object being passed from this module
#   output$testoutput3 <- renderPrint("This is the second input")
# }





