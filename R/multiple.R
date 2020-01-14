# Tab for multiple measurements ##############################################################

# UI
.multipleUI <- function(id, label="multiple measurements") {
  ns <- NS(id)

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Measurements to SDS"),
        
        conditionalPanel(
          condition = "output['multiple-uploaded'] == false",
          fileInput(ns('file'), 'Upload data file')
        ),
        
        conditionalPanel(
          condition = "output['multiple-uploaded'] == true",
          selectInput(ns("sex"),  label = "Sex",  choices = c('NOT POPULATED')),
          selectInput(ns("age_source"), "Age", c('NOT POPULATED')),
          selectInput(ns("age_unit"), "Unit of age", choices = c('Days', 'Weeks', 'Months', 'Years')),

          uiOutput(ns("measurement_inputs")),

          selectInput(ns("to_add"),
                      "Calculate",
                      c("SDS", "Centile", "% Predicted", "Predicted", "% CV", "Skewness"),
                      selected = "SDS",
                      multiple = TRUE,
                      selectize = TRUE),
          actionButton(ns('apply'), 'Apply'),
          downloadButton(ns("download_data"), "Download")
        )
      ),

      mainPanel(
        h3(textOutput(ns("growth_ref"))),
        DTOutput(ns("table"))
      )
    )
  )
}

# Server
.multiple <- function(input, output, session, globals) {
  # To use `renderUI` within modules, we need to wrap names with `ns()`
  ns <- session$ns

  # to hold the original uploaded data
  original_data <- reactiveValues(
    df = NULL,
    # `offset` is the index of the last column of the original data
    offset = 0,
    initialised = FALSE
  )
  
  # the original columns in the uploaded spreadhsheet
  original_columns <- reactiveVal(NULL)
  # list of column options
  column_options <- reactiveVal(NULL)

  # display the currently selected growth reference
  output$growth_ref <- renderText({
    globals$growthReferenceName
  })

  # create input tags for each measurement available in the selected growth reference
  output$measurement_inputs <- renderUI({
    choices <-  c('N/A', column_options())

    measures <- lapply(globals$growthReferenceMeasures,
                       function(measure) {
                         selectInput(ns(measure$code), paste0(measure$description, " (", measure$unit, ")"),
                                     choices = choices, selected = get_selected(measure$code, 'N/A'))
                       })
    do.call(tagList, measures)
  })


  # save uploaded data and update options
  observeEvent(input$file, {
    if(!is.null(input$file)) {
      df <- as.data.frame(read.csv(input$file$datapath))
      numeric_columns <- vapply(df, is.numeric, FUN.VALUE = logical(1))
      df[,numeric_columns] <- round(df[,numeric_columns],
                                    digits = globals$roundToDigits)
      original_columns(names(df))
      # columns in the dataframe are prefixed
      column_options(paste('[Column]', isolate(original_columns()), sep = ' '))
      original_data$df <- df
      original_data$offset <- ncol(original_data$df)
      original_data$initialised = TRUE
      uploaded = TRUE

      # update sex dropdown
      selected <- get_selected("sex", 'Male')
      choices <-  c('Male', 'Female', column_options())
      updateSelectInput(session, 'sex', choices = choices,  selected = selected)
      
      # update age
      selected <- get_selected("age", '')
      updateSelectInput(session, 'age_source', choices = column_options(), selected = selected)
      selected <- get_selected("age", 'Days')
    }
  })

  # outputs the datatable in the main panel
  output$table <- renderDT(datatable(original_data$df,
                                     filter = "top",
                                     rownames = FALSE,
                                     options = list(scrollX = TRUE,
                                                    sDom  = '<"top">lrt<"bottom">ip')))

  # fills the 'selected' argument for selectInput when column names matches a regular expression.
  # `code` is a short name for the item that we want to match in the list of columns
  get_selected <- function(code, default) {
    if (code == "age") {
      to_match = '[Aa]ge|[Yy]ears|[Dd]ays|[Ww]eeks'
    } else if (code == "sex") {
      to_match = '[Ss]ex|[Gg]ender'
    } else if (code == "ht") {
      to_match = "[Hh]eight"
    } else if (code == "wt") {
      to_match = "[Ww]eight"
    } else if (code == "bmi") {
      to_match = "BMI|bmi"
    } else if (code == "head") {
      to_match = "[Hh]ead"
    } else {
      return(default)
    }
    match <- original_columns() %>% isolate %>% stringr::str_detect(to_match) %>% which
    { if (length(match) > 0) paste('[Column]', original_columns()[match[1]]) else default }
  }

  # when the 'apply' button is clicked
  observeEvent(input$apply, {
    df <- original_data$df[seq(1, original_data$offset)]
    # Initialise `new_columns` as a dataframe with the same rows as the original
    # data, but with no columns
    new_columns = df[0]
    # loop over every measurement
    for (item in globals$growthReferenceMeasures) {
      new_columns <- do_calculation_for_measurement(item, df, new_columns)
    }
    original_data$df <- cbind(df, new_columns)
  })

  # returns the sex column or value
  get_sex <- function() {
    sex_selected <- isolate(input$sex)
    if (startsWith(sex_selected, '[Column]')) {
      df <- isolate(original_data$df)
      sex_column_or_value = df[, strsplit(sex_selected, split=" ")[[1]][2]]
    } else {
      sex_column_or_value = sex_selected
    }
    return(sex_column_or_value)
  }

  # returns the age column or value
  get_age <- function() {
      df <- isolate(original_data$df)
      age_column_or_value <- isolate(input$age_source)
      age_unit <- stringr::str_to_lower(isolate(input$age_unit))

      age_column = df[, strsplit(age_column_or_value, split=" ")[[1]][[2]]]

      # transform age into years if necessary
      if (age_unit != 'years') {
          args <- list()
          args[[ age_unit ]] <- age_column
          age_column <- do.call(.duration_in_years, args)
      }

      return(age_column)
  }

  # list of the various statistics and the associated function
  calculations <- list(
    list(name='SDS', column_name='SDS', func=.get_sds),
    list(name='Centile', column_name='Centile', func=.get_centile),
    list(name='% Predicted', column_name='PercPredicted', func=.get_perc_predicted),
    list(name='Predicted', column_name='Predicted', func=.get_predicted),
    list(name='% CV', column_name='PercCV', func=.get_perc_cv),
    list(name='Skewness', column_name='Skewness', func=.get_skewness)
  )

  do_calculation_for_measurement <- function(measurement, df, new_columns) {
      value <- input[[measurement$code]]
      input_name <- measurement$description
      code_name <- measurement$code
      if (!is.null(value) && value != 'N/A') {
        # a function that returns column name for a given statistics
        new_col <- function(stat, column, code_name) {
          paste('LMS', stat, column, code_name, sep = '_')
        }
        column = strsplit(value, split=" ")[[1]][2]
        sex_column_or_value <- get_sex()
        age_column_or_value <- get_age()

        lms_stats <- .measurement_to_scores(age_column_or_value, sex_column_or_value, code_name, df[, column], ref=globals$growthReference)

        # loop through each of the possible statistics and calculate if necessary
        for (calc in calculations) {
            # if the user asked for this statistics
            if (calc$name %in% input$to_add) {
              # calculate using the appropriate function and add a new column to the dataframe
              result <- calc$func(lms_stats)
              if (is.numeric(result)) {
                result <- round(result, digits = globals$roundToDigits)
              }
              new_columns[[new_col(calc$column_name, column, code_name)]] <- result
            }
        }

      }

      return(new_columns)
  }

  output$download_data <- downloadHandler(
    filename = 'lms_download.csv',
    content = function(file) {
      write.csv(original_data$df, file, row.names = FALSE)
    }
  )
  
  output$uploaded <- reactive(original_data$initialised)
  outputOptions(output, "uploaded", suspendWhenHidden = FALSE)
  

}
