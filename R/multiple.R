# Tab for multiple measurements ##############################################################

# UI
.multipleUI <- function(id, label="multiple measurements") {
  ns <- NS(id)

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Measurements to SDS"),

        uiOutput(ns("file_input")),

        # after uploading data, we show:
        uiOutput(ns("measurement_form"))
        ),

      mainPanel(
        div(style='height:600px; overflow-y: scroll', tableOutput(ns("table")))
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
    initialised = FALSE
  )

  # the original columns in the uploaded spreadhsheet
  original_columns <- reactiveVal(NULL)

  # upload the data and save a local copy of the dataframe
  observe({
    if(!is.null(input$file)) {
      df <- as.data.frame(read.csv(input$file$datapath))
      original_columns(names(df))
      original_data$df <- df
      original_data$initialised = TRUE
    }
  })

  # hide the file input once we've got the data frame saved locally
  output$file_input <- renderUI({
    if(is.null(original_data$df)) {
      return(fileInput(ns('file'), 'Upload data file'))
    } else {
      return(NULL)
    }
  })

  # outputs the dataframe in the main panel
  output$table <- renderTable(original_data$df)

  # fills the 'selected' argument for selectInput when column names matches a regular expression
  get_selected <- function(to_match, default) {
    match <- original_columns() %>% isolate %>% stringr::str_detect(to_match) %>% which
    { if (length(match) > 0) paste('[Column]', original_columns()[match[1]]) else default }
  }

  # display the input form, populating the options from the uploaded data
  output$measurement_form <- renderUI({
    # don't show the form if user hasn't uploaded data
    if (!original_data$initialised) return(NULL)

    output_tags <- tagList()

    # appends a tag to the output list
    add_tag <- function(t) {
      output_tags[[length(output_tags) + 1]] <<- t
    }

    # columns in the dataframe are prefixed
    column_options <- paste('[Column]', isolate(original_columns()), sep = ' ')

    # sex
    selected <- get_selected('[Ss]ex|[Gg]ender', 'Male')
    options <-  c('Male', 'Female', column_options)
    selectInput(ns("sex"),  label = "Sex",  choices = options,  selected = selected, multiple = FALSE) %>% add_tag

    # age
    selected <- get_selected('[Aa]ge|[Yy]ears|[Dd]ays|[Ww]eeks', '')
    selectInput(ns("age_source"), "Age", column_options, selected = selected) %>% add_tag
    selectInput(ns("age_unit"), "Unit of age", c('Days', 'Weeks', 'Months', 'Years'), selected = selected) %>% add_tag

    options <-  c('N/A', column_options)
    selectInput(ns("height"), "Height (cm)", options, selected = get_selected('[Hh]eight', 'N/A')) %>% add_tag
    selectInput(ns("weight"), "Weight (kg)", options, selected = get_selected(c('[Ww]eight'), 'N/A')) %>% add_tag
    selectInput(ns("bmi"), "BMI (kg/m^2)", options, selected = get_selected(c('BMI|bmi'), 'N/A')) %>% add_tag
    selectInput(ns("sitht"), "Sitting height (cm)", options, 'N/A') %>% add_tag
    selectInput(ns("leglen"), "Leg length (cm)", options, 'N/A') %>% add_tag

    selectInput(ns("to_add"),
                "Calculate",
                c("SDS", "Centile", "% Predicted", "Predicted", "% CV", "Skewness"),
                selected = "SDS",
                multiple = TRUE,
                selectize = TRUE) %>% add_tag
    actionButton(ns('apply'), 'Apply') %>% add_tag
    downloadButton(ns("download_data"), "Download") %>% add_tag

    output_tags
  })

  # when the 'apply' button is clicked
  observeEvent(input$apply, {
    df <- isolate(original_data$df)
    # loop over every measurement
    for (item in measurement_names) {
      df <- do_calculation_for_measurement(item$name, df)
    }
    original_data$df <- df
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

  # list of possible calculations available
  # TODO: populate from selected reference sheet
  measurement_names <- list(
    height=list(name='height', code='ht'),
    weight=list(name='weight', code='wt'),
    bmi=list(name='bmi', code='bmi'),
    sitht=list(name='sitht', code='sitht'),
    leglen=list(name='leglen', code='leglen')
  )

  # list of the various statistics and the associated function
  calculations <- list(
    list(name='SDS', column_name='SDS', func=.get_sds),
    list(name='Centile', column_name='Centile', func=.get_centile),
    list(name='% Predicted', column_name='PercPredicted', func=.get_perc_predicted),
    list(name='Predicted', column_name='Predicted', func=.get_predicted),
    list(name='% CV', column_name='PercCV', func=.get_perc_cv),
    list(name='Skewness', column_name='Skewness', func=.get_skewness)
  )

  do_calculation_for_measurement <- function(measurement_name, df) {
      value <- input[[measurement_name]]
      input_name <- measurement_names[[measurement_name]]$name
      code_name <- measurement_names[[measurement_name]]$code

      if (!is.null(value) && value != 'N/A') {
        # a function that returns column name for a given statistics
        new_col <- function(stat, column, code_name) {
          paste('LMS', stat, column, code_name, sep = '_')
        }
        column = strsplit(value, split=" ")[[1]][2]
        sex_column_or_value <- get_sex()
        age_column_or_value <- get_age()

        lms_stats <- .measurement_to_scores(age_column_or_value, sex_column_or_value, code_name, df[, column])

        # loop through each of the possible statistics and calculate if necessary
        for (calc in calculations) {
            # if the user asked for this statistics
            if (calc$name %in% input$to_add) {
                # calculate using the appropriate function and add a new column to the dataframe
                df[[new_col(calc$column_name, column, code_name)]] <- calc$func(lms_stats)
            }
        }

      } else {
        # remove old columns
        df <- df[, !colnames(df) %in% measurement_names[[measurement_name]]$columns]
        measurement_names[[measurement_name]]$columns <- c()
      }

      return(df)
  }

  output$download_data <- downloadHandler(
    filename = 'lms_download.csv',
    content = function(file) {
      write.csv(original_data$df, file, row.names = FALSE)
    }
  )

}

