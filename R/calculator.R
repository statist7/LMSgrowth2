source("R/functions.R", local = TRUE)

# Measurement to SDS calculator ###############################################
.calculatorUI <- function(id, label="calculator ui") {
  ns <- NS(id)
  
  fluidPage(
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        h3("Measurement to SDS"),
        radioButtons(ns("sex"), label = h4("Sex"),
                     choices = list("Male" = 1, "Female" = 2), 
                     selected = 1),
        radioButtons(ns("age_input"), choices = list("Age" = "age", "Dates" = "dates"), selected = "age", label = h4("Age"), inline=TRUE),
        conditionalPanel(
          condition = "input['calculator-age_input'] == 'age'",
          numericInput(ns("age_years"), "years", value="", min=0),
          numericInput(ns("age_months"), "months", value="", min=0),
          numericInput(ns("age_weeks"), "weeks", value="", min=0),
          numericInput(ns("age_days"), "days", value="", min=0)
        ),
        conditionalPanel(
          condition = "input['calculator-age_input'] == 'dates'",
          dateInput(ns("date_of_birth"), "Birth"),
          dateInput(ns("date_of_measurement"), "Measurement")
        ),
        h4("Measurements"),
        uiOutput(ns("measurementInputs"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        p("Selected growth reference:"),
        textOutput(ns("growth_ref")),
        verbatimTextOutput(ns("age_info")),
        uiOutput(ns("measurementOutputs"))
      )
    )
  )
}

# Calculator tab server #######################################################
.calculator <- function(input, output, session, globals) {
  ns <- session$ns
  
  exists_and_is_numeric <- function(name) {
    return(name %in% names(input) && is.numeric(input[[name]]))
  }
  
  age_in_years <- reactive({
    if (input$age_input == "age") {
      .duration_in_years(input$age_years, input$age_months, input$age_weeks, input$age_days)
    } else {
      .date_diff(input$date_of_measurement, input$date_of_birth)
    }
  })
  
  output$age_info <- renderText({
    if (is.numeric(age_in_years()) && age_in_years() > 0) {
      paste("Age:", age_in_years(), "years")  
    }
  })
  
  # display the currently selected growth reference
  output$growth_ref <- renderText({
    globals$growthReference
  })
  
  # create an input box for each measurement in the growth reference
  output$measurementInputs <- renderUI({
    measures <- lapply(globals$growthReferenceMeasures,
           function(measure) {
             # keep the current input value for the measure, if it exists
             current_value <- isolate(input[[measure$code]])
             if (is.null(current_value)) {
               current_value <- measure$default
             }
             
             numericInput(ns(measure$code), 
                          paste0(measure$description, " (", measure$unit, ")"),
                          value=current_value,
                          min=measure$min,
                          max=measure$max)
           })
    do.call(tagList, measures)
  })
  
  # add an output box for each measurement in the growth reference
  output$measurementOutputs <- renderUI({
    measures <- lapply(globals$growthReferenceMeasures,
                             function(measure) {
                               output_name <- paste0(measure$code, "_info")
                               verbatimTextOutput(ns(output_name))
                             })
    do.call(tagList, measures)
  })
  
  # set up the reactive output for each measurement in the growth reference
  observeEvent(globals$growthReferenceMeasures, 
               for (measure in sapply(globals$growthReferenceMeasures, function(m) { m$code })) {
                 local({
                   input_name <- measure
                   output_name <- paste0(input_name, "_info")
                   output[[output_name]] <- renderText({
                     if (is.numeric(input[[input_name]])) {
                       lms_stats <- .measurement_to_scores(age_in_years(), input$sex, input_name, input[[input_name]], globals$growthReference)
                       .stats2string(lms_stats, output_name)
                     }
                   })
                 })
               }
  )


  # autofill bmi if height and weight are given
  set_bmi <- function() {
    if (exists_and_is_numeric("ht") && exists_and_is_numeric("wt")) {
        bmi <- input$wt/(input$ht/100)^2
        updateNumericInput(session, "bmi", value=bmi)
      }
  }
  observeEvent(c(input$ht, input$wt), { set_bmi() })
  
  # autofill leglen if height and sitting height are given
  set_leglen <- function() {
    if (exists_and_is_numeric("ht") && exists_and_is_numeric("sitht")) {
        leglen <- input$ht - input$sitht
        updateNumericInput(session, "leglen", value=leglen)
    }
  }
  observeEvent(c(input$ht, input$sitht), { set_leglen() })
}
