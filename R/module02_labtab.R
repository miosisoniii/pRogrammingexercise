#-------------------------------------------------------------------------------------#
# Project: Module02: Plot Tab
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
      column(width = 7,
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
             # checkboxGroupInput(ns("category"), "Test Category: ",
             #                    choices = unique(cast_dat$LBCAT),
             #                    selected = unique(cast_dat$LBCAT)
             # ),
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
  
  ## create reactives and output in list format to be used in other modules
  inputselection <- reactive({
    armfilt <- input$arm
    racefilt <- input$race
    labtestfilt <- input$labtest
    # categoryfilt <- input$category
    sexfilt <- input$sex
    agefilt1 <- input$age[1]
    agefilt2 <- input$age[2]
    varlist <- list(armfilt, racefilt, labtestfilt, sexfilt, agefilt1, agefilt2)
    # assign names to the variable list in the reactive
    names(varlist) <- c("arm", "race", "labtest", "sex", "age1", "age2")
    return(varlist)
  })
  
  ## filtered dataframe
  datafiltered <- reactive({
    data_out <- melt_dat %>%
      filter(SEX %in% input$sex,
             RACE %in% input$race,
             AGE %in% seq(input$age[1], input$age[2]),
             ACTARM %in% input$arm,
             LBTEST %in% input$labtest)
    return(data_out)
  })

  ## return reactive inputs as a list
  list(
    inputselection, # reactive input selection list format
    datafiltered    # reactive dataframe output
  )
}

#-------------------------------------------------------------------------------------#
# DATATABLE OUTPUT MODULE
# This module takes the input module filtered output and displays it in the Data Table tab
#-------------------------------------------------------------------------------------#

outputmodule_UI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(NS(id, "datafilt_output"))
  )
}

outputmodule_Server <- function(input, output, session, object) {
  output$datafilt_output <- renderDataTable(object[[2]]())
}

#-------------------------------------------------------------------------------------#
# PLOT MODULE
# This module takes the input module filtered output and displays it in the Plot Tab
#-------------------------------------------------------------------------------------#

plotmodule_UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(NS(id, "labplotoutput1")),
    plotOutput(NS(id, "labplotoutput2")),
    plotOutput(NS(id, "labplotoutput3")),
    plotOutput(NS(id, "patientplotoutput1")),
    plotOutput(NS(id, "patientplotoutput2"))
  )
}

plotmodule_Server <- function(input, output, session, object) {

  ### renders the plot output data
  output$labplotoutput1 <- renderPlot(
    ggplot(data = object[[2]](),
            aes(x = VISIT, y = RESULT, color = ACTARM)) +
      facet_wrap(~BMRKR2) + 
      geom_boxplot() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1.0),
            legend.position = "bottom") + 
      ggtitle(paste0("Laboratory Test: ", object[[1]]()[3], "\nStratified by Biomarker Classification")) # calls the selection of a single measurement
    )
  
  ### renders the plot output data
  output$labplotoutput2 <- renderPlot(
    ggplot(data = object[[2]](),
           aes(x = VISIT, y = RESULT, color = ACTARM)) +
      geom_boxplot() + 
      facet_wrap(~SEX) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.0),
            legend.position = "bottom") + 
      ggtitle(paste0("Laboratory Test: ", object[[1]]()[3], "\nStratified by Sex")) # calls the selection of a single measurement
  )
  
  ### plotting mean patient response
  output$labplotoutput3 <- renderPlot(
    ggplot(data = object[[2]]() %>%
             filter(VISIT %in% c("PEAK", "SCRN_DELTA", "WK1_DELTA", "WK2_DELTA", "WK3_DELTA",
                                 "WK4_DELTA", "WK5_DELTA", "PEAK_DELTA") == FALSE) %>%
             group_by(VISIT, ACTARM) %>%
             summarize(MEAN = mean(RESULT),
                       SD = sd(RESULT)),
           aes(x = VISIT, y = MEAN, fill = ACTARM)) +
      # facet_wrap(~ACTARM, scales = "free") +
      geom_bar(stat = "identity", 
               color = "black",
               position = position_dodge()) +
      geom_errorbar(aes(ymin = MEAN - SD, 
                        ymax = MEAN + SD),
                    width = 0.2,
                    position = position_dodge(0.9)) +
      theme(axis.text.x = element_text(angle = 45, 
                                       hjust = 1.0),
            legend.position = "right") +
      # coord_cartesian(ylim = c(0, 75))+
      ggtitle(paste0("Laboratory Test (Mean): ", object[[1]]()[3])) # calls the selection of a single measurement

  )  
  
  ### plotting patient data plot - by Study Arm
  output$patientplotoutput1 <- renderPlot(
    ggplot(data = object[[2]](),
           aes(x = AGE, fill = SEX)) +
      geom_histogram(alpha = 0.6,  position = "dodge") +
      facet_wrap(~ ACTARM) +
      scale_fill_manual(values = c("#69b3a2", "#404080")) +
      theme(axis.text.x = element_text(hjust = 1.0)) +
      ggtitle("Patient Age Distribution\nStratified Across Study Arm")
  )
  
  ### plotting patient data plot - by biomarker classification
  output$patientplotoutput2 <- renderPlot(
    ggplot(data = object[[2]](),
           aes(x = AGE, fill = SEX)) +
      geom_histogram(alpha = 0.6,  position = "dodge") +
      facet_wrap(~ BMRKR2) +
      scale_fill_manual(values = c("#69b3a2", "#404080")) +
      theme(axis.text.x = element_text(hjust = 1.0)) +
      ggtitle("Patient Age Distribution\nStratified Across Biomarker Classification")
  )
  
}






