source("R/functions.R", local = TRUE)

# Measurement to SDS calculator ###############################################
.calculatorUI <- function(id, label="calculator ui") {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Single date calculator"),
        radioButtons(ns("sex"), label = h4("Sex"), choices = list("Male" = 1, "Female" = 2),  selected = 1, inline = T),
        radioButtons(ns("age_setting"), choices = list("Age" = "age", "Date" = "dates"), selected = "age", label = h4("Age"), inline=TRUE),
        conditionalPanel(
          condition = "input['calculator-age_setting'] == 'age'",
          fluidRow(
            column(6,  numericInput(ns("age_years"), "Years", value="", min=0), numericInput(ns("age_weeks"), "Weeks", value="", min=0)),
            column(6,  numericInput(ns("age_months"), "Months", value="", min=0), numericInput(ns("age_days"), "Days", value="", min=0))
          )
        ),
        conditionalPanel(
          condition = "input['calculator-age_setting'] == 'dates'",
          dateInput(ns("date_of_birth"), "Birth"),
          dateInput(ns("date_of_measurement"), "Measurement")
        ),
        checkboxInput(ns("adjust_gestation"), label = "Adjust for gestational age",
                      value = FALSE),
        conditionalPanel(
          condition = "input['calculator-adjust_gestation']",
          numericInput(ns("gestational_age"), label = "Gestational age (weeks)",
                       value = 40, min = 0, max = 100)
        ),
        h4("Measurements"),
        uiOutput(ns("measurement_inputs")),
        h4("Plot options"),
        radioButtons(ns("plot_axis_select"), label = NULL, choices = list("Centile", "z-score"), selected = "Centile", inline = T)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(textOutput(ns("growth_ref"))),      
        h4(textOutput(ns("age_info"))),
        formattableOutput(ns("measurement_table")),
        plotlyOutput(ns("measurement_plot"))
      )
    )
  )
}

# Measurement to SDS calculator server #######################################################
.calculator <- function(input, output, session, globals) {
  ns <- session$ns
  
  age_in_years <- reactive({
    # either the age was specified directly
    if (input$age_setting == "age") {
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
    globals$growthReferenceName
  })
  
  # Hold a list of lists of all possible measurements - some will be calculated, others not
  calculated_measurements <- reactiveValues()
  
  # Holds the dataframe of measurement statistics that are displayed on the screen
  calculated_statistics <- reactiveVal(value = NULL)
  
  # create input tags for each measurement available in the selected growth reference
  output$measurement_inputs <- renderUI({
    # keep an ordered list of measurements
    calculated_measurements[['.codes']] <- globals$growthReferenceMeasures %>% map_chr(function(x) { x[['code']] })
    
    measures <- lapply(globals$growthReferenceMeasures,
           function(measure) {
             current_value <- isolate(input[[measure$code]])
             if (!is.numeric(current_value)) {
               current_value <- measure$default
               calculated_measurements[[measure$code]] <- list(sds=NA, centile=NA, pred=NA, pred_perc=NA, cv_perc=NA, skewness=NA, code=measure$code)
             }
             
             numericInput(ns(measure$code), 
                          paste0(measure$description, " (", measure$unit, ")"),
                          value=current_value,
                          min=measure$min,
                          max=measure$max)
             
           })
    do.call(tagList, measures)
  })
  
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
  
  validate_input <- function() {
    if (age_in_years() == 0) {
      return("Enter child's age at date of measurement.")
    } else if (age_in_years() > globals$growthReferenceAgeStop) {
      return("Age is out-of-range for selected growth reference.")
    }
    NULL
  }
  
  # build the dataframe that holds measurement statistics to display to user
  observe({
    validate(
      validate_input()
    )
    # get the available measurements (depends on selected growth reference) and put them in a dataframe
    available_measurements <- reactiveValuesToList(calculated_measurements, all.names=FALSE)
    df <- data.frame(matrix(unlist(available_measurements), ncol=7, byrow=T), stringsAsFactors = FALSE)
    # prevent missing measurements being included (e.g. when you switch reference)
    colnames(df) <- c('SDS', 'Centile', 'Predicted', '% Predicted', '% CV', 'Skewness', 'code')
    df <- df[df$code %in% calculated_measurements$.codes,]
    
    # all the columns are double except 'code' and 'Centile' columns (keep them as string)
    df <- df %>% tbl_df %>% 
      mutate_at(grep("code|Centile", colnames(.), invert=T), funs(as.numeric)) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      as.data.frame()
    
    # order the columns by the order they appear in the growth reference/input boxes
    df <- df[order(match(df$code, calculated_measurements$.codes)), ]
    
    # use the descriptive name of measurement
    df$description <- sapply(calculated_measurements$.codes, 
                             function(x1) { detect(globals$growthReferenceMeasures, function(x2) { x2[['code']] == x1})$description })
    
    # remove all measurements that don't have calculations
    df <- df[!(is.na(df$SDS)), ]
    
    # don't display anything if there aren't any stats
    if (nrow(df) == 0) {
      calculated_statistics(NULL)
    }
    
    row.names(df) <- df$description
    df$code <- NULL
    df$description <- NULL
    
    calculated_statistics(df)
  })
  
  output$measurement_table <- renderFormattable({
    validate(
      validate_input()
    )
    if (is.data.frame(calculated_statistics()) &&  nrow(calculated_statistics()) > 0) {
      formattable(calculated_statistics(), list())
    } else {
      formattable(data.frame(), list())
    }
  })
  
  # make observeEvents for each measurement available in the growth reference
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
            c(input[[input_name]], age_in_years(), input$sex,
              input$adjust_gestation, input$gestational_age), {
              # if the input is numerical value TODO: change to req()
              if (is.numeric(input[[input_name]])) {
                # a helper function to update calculated measurements
                update_mc <- function(name, value) {
                  calculated_measurements[[input_name]][[name]] <- value
                }
                
                # get and set the lms statistics for this measure
                if (input$adjust_gestation) {
                  age <- .adjust_age(age_in_years(), input$gestational_age)
                } else {
                  age <- age_in_years()
                }
                lms_stats <- .measurement_to_scores(age, input$sex, input_name, input[[input_name]], globals$growthReferenceData)
                update_mc('sds', .get_sds(lms_stats))
                update_mc('centile', .get_centile(lms_stats))
                update_mc('pred', .get_predicted(lms_stats))
                update_mc('pred_perc', .get_perc_predicted(lms_stats))
                update_mc('cv_perc', .get_perc_cv(lms_stats))
                update_mc('skewness', .get_skewness(lms_stats))
              } else {
                # this measurement has not been set (SDS value is NA, the rest don't matter)
                calculated_measurements[[input_name]] <- list(sds=NA, centile=NA, pred=NA, pred_perc=NA, cv_perc=NA, skewness=NA, code=input_name)
              }
          }) # end observeEvent()
        }) # end local()
      }
    }
  )
  
  output$measurement_plot <- renderPlotly({
    validate(
      validate_input()
    )
    
    if (!is.data.frame(calculated_statistics()) || nrow(calculated_statistics()) == 0) {
      return(NULL)
    }
    
    default_centiles <- c(2, 9, 25, 50, 75, 91, 98)  # removed 0.4, 99.6, overlapping
    
    df <- calculated_statistics()
    
    measurement_names <- rownames(df)
    measurement_names <- factor(measurement_names, levels=measurement_names)
    
    if (input$plot_axis_select == "z-score") {
      measurement_y <- df$SDS
      range <- c(floor(min(df$SDS)) - 1, ceiling(max(df$SDS)) + 1)
      tickvals <- seq(range[1], range[2])
    } else {
      measurement_y <- df$Centile
      # there might be some which are SDS, fix those entries to 100th or 0th centile
      measurement_y[stringr::str_detect(df$Centile, 'SDS\\+')] <- "100th"
      measurement_y[stringr::str_detect(df$Centile, 'SDS\\-')] <- "0th"
      # get rid of letters in centile for co-ordinates
      measurement_y <- as.numeric(stringr::str_replace_all(measurement_y, "([A-z])", ""))
      range <- c(-2, 102)
      tickvals <- default_centiles
    }
    
    label <- input$plot_axis_select
    
    plot_ly(x = ~measurement_names, 
            y = ~measurement_y, 
            type="scatter", 
            mode="markers", 
            marker = list(size = 10), 
            hoverinfo = "text", 
            hovertext = paste0("(", measurement_names, "; z = ", df$SDS, "; centile = ", df$Centile,  ")")
            ) %>% 
      layout(yaxis = list(autorange = FALSE, 
                          range = range,
                          tickvals = tickvals,
                          tickmode = "array",
                          ticktext = as.character(tickvals),
                          title = list(text=label),
                          zeroline = FALSE),
             xaxis = list(title = list(text="Measurement")))
  })
}
