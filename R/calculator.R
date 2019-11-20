source("R/functions.R", local = TRUE)

# Measurement to SDS calculator ###############################################
.calculatorUI <- function(id, label="calculator ui") {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Measurement to SDS"),
        radioButtons(ns("sex"), label = h4("Sex"), choices = list("Male" = 1, "Female" = 2),  selected = 1, inline = T),
        radioButtons(ns("age_input"), choices = list("Age" = "age", "Dates" = "dates"), selected = "age", label = h4("Age"), inline=TRUE),
        conditionalPanel(
          condition = "input['calculator-age_input'] == 'age'",
          fluidRow(
            column(6,  numericInput(ns("age_years"), "Years", value="", min=0), numericInput(ns("age_weeks"), "Weeks", value="", min=0)),
            column(6,  numericInput(ns("age_months"), "Months", value="", min=0), numericInput(ns("age_days"), "Days", value="", min=0))
          )
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
        h3(textOutput(ns("growth_ref"))),      
        h4(textOutput(ns("age_info"))),
        formattableOutput(ns("measurementTable"))
      )
    )
  )
}

# Measurement to SDS calculator server #######################################################
.calculator <- function(input, output, session, globals) {
  ns <- session$ns
  
  age_in_years <- reactive({
    # either the age was specified directly
    if (input$age_input == "age") {
      y <- .duration_in_years(input$age_years, input$age_months, input$age_weeks, input$age_days)
    } else {
      # or calculate age using dob and date of measurement
      y <- .date_diff(input$date_of_measurement, input$date_of_birth)
    }
    round(y, 2)
  })
  
  output$age_info <- renderText({
    if (is.numeric(age_in_years()) && age_in_years() > 0) {
      if (input$sex == 1) {
        sex_str <- "Male"
      } else {
        sex_str <- "Female"
      }
      paste(sex_str, ", ", age_in_years(), " years", sep="")  
    }
  })
  
  # display the currently selected growth reference
  output$growth_ref <- renderText({
    references <- .get_references()
    reference_name <- names(references)[references == globals$growthReference]
    return(reference_name)
  })
  
  # Each row of the output dataframe is stored as a list in this reactiveValue
  measurementCalculated <- reactiveValues()
  
  # create an input box for each measurement in the growth reference
  output$measurementInputs <- renderUI({
    # keep an ordered list of measurements
    measure_codes <- globals$growthReferenceMeasures %>% map_chr(function(x) { x[['code']] })
    measurementCalculated[['.codes']] <- measure_codes
    
    measures <- lapply(globals$growthReferenceMeasures,
           function(measure) {
             measurementCalculated[[measure$code]] <- list(sds=NA, centile=2, pred=3, pred_perc=4, cv_perc=5, skewness=6, code=measure$code)
             
             # TODO: this is not working, output is not refreshed when reference changed
             # keep the current input value for the measure, if it exists
             # current_value <- isolate(input[[measure$code]])
             # if (is.null(current_value)) {
             #   current_value <- measure$default
             # }
             
             numericInput(ns(measure$code), 
                          paste0(measure$description, " (", measure$unit, ")"),
                          value=measure$default,
                          min=measure$min,
                          max=measure$max)
             
           })
    do.call(tagList, measures)
  })
  
  output$measurementTable <- renderFormattable({
    # get the available measurements (depends on selected growth reference) and put them in a dataframe
    availableMeasurements <- reactiveValuesToList(measurementCalculated, all.names=FALSE)
    df <- data.frame(matrix(unlist(availableMeasurements), nrow=length(availableMeasurements), byrow=T), stringsAsFactors = FALSE)
    
    # prevents stale measurements being includes (e.g. when you switch reference)
    colnames(df) <- c('SDS', 'Centile', 'Predicted', '% Predicted', '% CV', 'Skewness', 'code')
    df <- df[df$code %in% measurementCalculated$.codes,]
    
    # all the columns are double except 'code' column (keep as string)
    df <- df %>% tbl_df %>% mutate_at(grep("code|Centile", colnames(.), invert=T), funs(as.numeric)) %>% as.data.frame()
    
    # order the columns by the order they appear in the growth references (and so the input boxes)
    df <- df[order(match(df$code, measurementCalculated$.codes)), ]
    
    # use the descriptive name of measurement
    df$description <- sapply(measurementCalculated$.codes, 
                             function(x1) { detect(globals$growthReferenceMeasures, function(x2) { x2[['code']] == x1})$description })
    
    # remove all measurements that don't have calculations
    df <- df[!(is.na(df$SDS)), ]
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    # round all numeric columns to 2 decimal places
    df <- df %>% tbl_df %>% mutate_if(is.numeric, round, 2) %>% as.data.frame()
    row.names(df) <- df$description
    df$code <- NULL
    df$description <- NULL
    formattable(df, list())
  })
  
  # watch for changes in the measure available in selected growth reference
  observeEvent(
    globals$growthReferenceMeasures, {
      # for each measure code in the current growth measure
      for (measure in sapply(globals$growthReferenceMeasures, function(m) { m$code })) {
        # we need local() because we're going to create new observeEvent() for each measure
        local({
          input_name <- measure
          output_name <- paste0(input_name, "_info")
          
          # when the measure input box changes
          observeEvent(
            c(input[[input_name]], age_in_years()), { 
              # if the input is numerical value TODO: change to req()
              if (is.numeric(input[[input_name]])) {
                # a helper function to update calculated measurements
                update_mc <- function(name, value) {
                  measurementCalculated[[input_name]][[name]] <- value
                }
                
                # get and set the lms statistics for this measure
                lms_stats <- .measurement_to_scores(age_in_years(), input$sex, input_name, input[[input_name]], globals$growthReference)
                update_mc('sds', .get_sds(lms_stats))
                update_mc('centile', .get_centile(lms_stats))
                update_mc('pred', .get_predicted(lms_stats))
                update_mc('pred_perc', .get_perc_predicted(lms_stats))
                update_mc('cv_perc', .get_perc_cv(lms_stats))
                update_mc('skewness', .get_skewness(lms_stats))
              } else {
                # this measurement has not been set (SDS value is NA, the rest don't matter)
                measurementCalculated[[input_name]]$sds <- NA
              }
          }) # end observeEvent()
        }) # end local()
      }
    }
  )
  
  exists_and_is_numeric <- function(name) {
    return(name %in% names(input) && is.numeric(input[[name]]))
  }

  # autofill bmi if height and weight are given
  set_bmi <- function() {
    if (exists_and_is_numeric("ht") && exists_and_is_numeric("wt")) {
        bmi <- input$wt/(input$ht/100)^2
        bmi <- round(bmi, 2)
        updateNumericInput(session, "bmi", value=bmi)
      }
  }
  observeEvent(c(input$ht, input$wt), { set_bmi() })
  
  # autofill leglen if height and sitting height are given
  set_leglen <- function() {
    if (exists_and_is_numeric("ht") && exists_and_is_numeric("sitht")) {
        leglen <- input$ht - input$sitht
        leglen <- round(leglen, 2)
        updateNumericInput(session, "leglen", value=leglen)
    }
  }
  observeEvent(c(input$ht, input$sitht), { set_leglen() })
}
