source("R/functions.R", local = TRUE)

# Measurement to SDS calculator ###############################################
.calculatorUI <- function(id, label="calculator ui") {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Single date calculator"),
        radioButtons(ns("sex"), label = h4("Sex"), choices = list("Male" = 1, "Female" = 2),  selected = 1, inline = T),
        radioButtons(ns("age_input"), choices = list("Age" = "age", "Date" = "dates"), selected = "age", label = h4("Age"), inline=TRUE),
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
        uiOutput(ns("measurementInputs")),
        h4("Plot options"),
        radioButtons(ns("plotYAxis"), label = NULL, choices = list("Centile", "z-score"), selected = "Centile", inline = T)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(textOutput(ns("growth_ref"))),      
        h4(textOutput(ns("age_info"))),
        formattableOutput(ns("measurementTable")),
        plotlyOutput(ns("measurementPlot"))
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
    round(y, globals$roundToDigits)
  })
  
  output$age_info <- renderText({
    if (is.numeric(age_in_years()) && age_in_years() > 0) {
      if (input$sex == 1) {
        sex_str <- "Male"
      } else {
        sex_str <- "Female"
      }
      paste0(sex_str, ", ", age_in_years(), " years")
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
    measurementCalculated[['.codes']] <- globals$growthReferenceMeasures %>% map_chr(function(x) { x[['code']] })
    
    # TODO: reset measurements that don't exist
    
    measures <- lapply(globals$growthReferenceMeasures,
           function(measure) {
             current_value <- isolate(input[[measure$code]])
             if (!is.numeric(current_value)) {
               current_value <- measure$default
               measurementCalculated[[measure$code]] <- list(sds=NA, centile=NA, pred=NA, pred_perc=NA, cv_perc=NA, skewness=NA, code=measure$code)
             }
             
             numericInput(ns(measure$code), 
                          paste0(measure$description, " (", measure$unit, ")"),
                          value=current_value,
                          min=measure$min,
                          max=measure$max)
             
           })
    do.call(tagList, measures)
  })
  
  validate_input <- function() {
    if (age_in_years() == 0) {
      return("Enter child's age at date of measurement.")
    } else if (age_in_years() > globals$growthReferenceAgeStop) {
      return("Age is out-of-range for selected growth reference.")
    }
    NULL
  }
  
  calculatedStats <- reactiveVal(value = NULL)
  
  observe({
    validate(
      validate_input()
    )
    # get the available measurements (depends on selected growth reference) and put them in a dataframe
    availableMeasurements <- reactiveValuesToList(measurementCalculated, all.names=FALSE)
    df <- data.frame(matrix(unlist(availableMeasurements), ncol=7, byrow=T), stringsAsFactors = FALSE)
    # prevent missing measurements being included (e.g. when you switch reference)
    colnames(df) <- c('SDS', 'Centile', 'Predicted', '% Predicted', '% CV', 'Skewness', 'code')
    df <- df[df$code %in% measurementCalculated$.codes,]
    
    # all the columns are double except 'code' and 'Centile' columns (keep them as string)
    df <- df %>% tbl_df %>% 
      mutate_at(grep("code|Centile", colnames(.), invert=T), funs(as.numeric)) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      as.data.frame()
    
    # order the columns by the order they appear in the growth reference/input boxes
    df <- df[order(match(df$code, measurementCalculated$.codes)), ]
    
    # use the descriptive name of measurement
    df$description <- sapply(measurementCalculated$.codes, 
                             function(x1) { detect(globals$growthReferenceMeasures, function(x2) { x2[['code']] == x1})$description })
    
    # remove all measurements that don't have calculations
    df <- df[!(is.na(df$SDS)), ]
    
    # don't display anything if there aren't any stats
    if (nrow(df) == 0) {
      calculatedStats(NULL)
    }
    
    row.names(df) <- df$description
    df$code <- NULL
    df$description <- NULL
    
    calculatedStats(df)
  })
  
  output$measurementTable <- renderFormattable({
    validate(
      validate_input()
    )
    if (is.data.frame(calculatedStats()) &&  nrow(calculatedStats()) > 0) {
      formattable(calculatedStats(), list())
    } else {
      formattable(data.frame(), list())
    }
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
            c(input[[input_name]], age_in_years(), input$sex), { 
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
                measurementCalculated[[input_name]] <- list(sds=NA, centile=NA, pred=NA, pred_perc=NA, cv_perc=NA, skewness=NA, code=input_name)
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
        bmi <- round(bmi, globals$roundToDigits)
        updateNumericInput(session, "bmi", value=bmi)
      }
  }
  observeEvent(c(input$ht, input$wt), { set_bmi() })
  
  # autofill leglen if height and sitting height are given
  set_leglen <- function() {
    if (exists_and_is_numeric("ht") && exists_and_is_numeric("sitht")) {
        leglen <- input$ht - input$sitht
        leglen <- round(leglen, globals$roundToDigits)
        updateNumericInput(session, "leglen", value=leglen)
    }
  }
  observeEvent(c(input$ht, input$sitht), { set_leglen() })
  
  output$measurementPlot <- renderPlotly({
    validate(
      validate_input()
    )
    
    if (!is.data.frame(calculatedStats()) || nrow(calculatedStats()) == 0) {
      return(NULL)
    }
    
    default_centiles <- c(2, 9, 25, 50, 75, 91, 98)  # removed 0.4, 99.6, overlapping
    default_z <- c(-3, -2, -1, 0, 1, 2, 3)
    
    df <- calculatedStats()
    
    measurement_names <- rownames(df)
    measurement_names <- factor(measurement_names, levels=measurement_names)
    
    if (input$plotYAxis == "z-score") {
      measurement_y <- df$SDS
      range <- c(default_z[1], default_z[length(default_z)])
      tickvals <- default_z
    } else {
      measurement_y <- as.numeric(gsub("([0-9]+).*$", "\\1", df$Centile))
      range <- c(default_centiles[1], default_centiles[length(default_centiles)])
      tickvals <- default_centiles
    }
    
    label <- input$plotYAxis
    
    plot_ly(x = ~measurement_names, y = ~measurement_y, type="scatter", mode="markers", marker = list(size = 10)) %>% 
      layout(yaxis = list(autorange = FALSE, 
                          range = range,
                          tickvals = tickvals,
                          tickmode = "array",
                          ticktext = as.character(tickvals),
                          title = list(text=label)),
             xaxis = list(title = list(text="Measurement")))
  })
}
