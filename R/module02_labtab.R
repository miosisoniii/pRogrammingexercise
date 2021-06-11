#-------------------------------------------------------------------------------------#
# Project: Module02: Lab Breakdown Tab
# Purpose: 
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
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
  
  ## return reactive inputs as a list
  list(
    inputselection = reactive({
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
    }),
    datafiltered = reactive({input$race})
  )

  
  ## filter the lab data using the reactive above
  # datafilt <- reactive({
  #   data_out <- melt_dat %>%
  #     filter(SEX %in% varSelect()$sex,
  #            RACE %in% varSelect()$race,
  #            AGE %in% seq(varSelect()$age1, varSelect()$age2),
  #            ACTARM %in% varSelect()$arm,
  #            LBTEST %in% varSelect()$labtest,
  #            LBCAT %in% varSelect()$category
  #     )
  # })

}

outputmodule_UI <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(NS(id, "varselect_output")),
    renderDataTable(NS(id, "datafilt_output"))
             
  )
}

outputmodule_Server <- function(input, output, session, object) {
  ## test returned output
  output$varselect_output <- renderPrint(object$inputselection())
  
  
  
  
  # this works! that means the output module server is talking to it's UI AND
  # calling the function in the other module works
  # output$varselect_output <- renderPrint("maybe?")
  
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





