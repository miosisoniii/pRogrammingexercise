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
             radioButtons(ns("labtest"), "Laboratory Test: ",
                          choices = unique(cast_dat$LBTEST),
                          selected = "Alanine Aminotransferase Measurement"
             ),
             checkboxGroupInput(ns("arm"), "Select Study Arm: ",
                                choices = unique(cast_dat$ACTARM),
                                selected = unique(cast_dat$ACTARM)
             ),
             checkboxGroupInput(ns("race"), "Patient Race: ",
                                choices = unique(cast_dat$RACE),
                                selected = unique(cast_dat$RACE)
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
  # output$varselect_output <- renderPrint(object$inputselection())
  # output$datafilt_output <- renderDataTable(object$datafiltered())
  output$datafilt_output <- renderDataTable(object[[2]]())
}

#-------------------------------------------------------------------------------------#
# PLOT MODULE
#-------------------------------------------------------------------------------------#

plotmodule_UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(NS(id, "labplotoutput")),
    plotOutput(NS(id, "patientplotoutput")),
    verbatimTextOutput(NS(id, "testtextoutput")),
    dataTableOutput(NS(id, "testtableoutput"))
  )
}

plotmodule_Server <- function(input, output, session, object) {

  ### renders the plot output data
  output$labplotoutput <- renderPlot(    # plotting output
    ggplot(data = object[[2]](),
            aes(x = VISIT, y = RESULT, color = ACTARM, group = LBTEST)) +
      geom_jitter() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1.0),
            legend.position = "bottom") + 
      ggtitle(paste0("Laboratory Test Measurements\n", object[[1]]()[3])) # calls the selection of a single measurement
    )
  
  ### plotting patient data plot
  ## inverted histogram plot reference: https://www.r-graph-gallery.com/density_mirror_ggplot2.html
  output$patientplotoutput <- renderPlot(    # plotting output
    ggplot(data = object[[2]](),
           aes(x = AGE, fill = SEX)) +
      geom_histogram(color = "#e9ecef", alpha = 0.6,  position = "dodge") +
      scale_fill_manual(values = c("#69b3a2", "#404080")) +
      theme(axis.text.x = element_text(hjust = 1.0)) +
      ggtitle("Patient Age Distribution")
    ## inverted histogram code
    # ggplot(data = object[[2]](),
    #        aes(x = x)) +
    #   geom_histogram(aes(x = BMRKR1, y = ..density.., fill = SEX), 
    #                  color = "#e9ecef", alpha = 0.6,  position = "dodge") +
    #   geom_histogram(aes(x = AGE, y = -..density.., fill = SEX), 
    #                  color = "#e9ecef", alpha = 0.6,  position = "dodge") +
    #   scale_fill_manual(values = c("#69b3a2", "#404080")) +
    #   geom_label(aes(x = 4.5, y = 0.25, label = "Biomarker Measurement")) + 
    #   geom_label(aes(x = 4.5, y = -0.25, label = "Patient Age")) + 
    #   theme(axis.text.x = element_text(hjust = 1.0)) +
    #   ggtitle("Patient Age Distribution")
  )
  
  
  
  
  
  ## output code that works
  output$testtextoutput <- renderPrint(object[[1]]()) # selection output in text format
  output$testtableoutput <- renderDataTable(object[[2]]()) # selection filtered datatable output
  
}






