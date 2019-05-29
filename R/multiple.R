# Tab for multiple measurements ##############################################################
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

# Server logic for multiple measurements ########################################################
.multiple <- function(input, output, session, stringAsFactors) {
  # To use `renderUI` within modules, we need to wrap with `ns()`
  ns <- session$ns
  
  original_data <- reactiveValues(
    df = NULL,
    initialised = FALSE
  )
  
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
  
  output$table <- renderTable(original_data$df)
  
  # display the input form, populating the options from the uploaded data
  output$measurement_form <- renderUI({
    if (!original_data$initialised) return(NULL)
    
    output_tags <- tagList()
    
    add_tag <- function(t) {
      output_tags[[length(output_tags) + 1]] <<- t 
    }
    
    column_options <- paste('[Column]', isolate(original_columns()), sep = ' ')
    
    # Sex
    match <- intersect(c('Sex', 'sex', 'Gender', 'gender'), isolate(original_columns()))
    if (length(match) == 1) {
      selected <- paste('[Column] ', match, sep='')
    } else {
      selected <- 'Male'
    }
    
    options <-  c('Male', 'Female', column_options)
    
    add_tag(selectInput(ns("sex"),  label = "Sex",  choices = options,  selected = selected, multiple = FALSE))
    
    # Age    
    match <- intersect(c('Age', 'age', 'Years', 'years', 'Days', 'days'), isolate(original_columns()))
    
    if (length(match) == 1) {
      selected <- paste('[Column] ', match, sep='')
    } else if (length(match) > 1) {
      selected <- paste('[Column] ', match[1], sep='')
    } else {
      selected <- ''
    }
    
    add_tag(selectInput(ns("age_source"), "Age", column_options, selected = selected))
    add_tag(selectInput(ns("age_unit"), "Unit of age", c('Days', 'Weeks', 'Months', 'Years'), selected = 'years'))
    
    # only display measurement selection if we have loaded a dataframe
    options <-  c('N/A', column_options)
    add_tag(selectInput(ns("height"), "Height (cm)", options))
    add_tag(selectInput(ns("weight"), "Weight (kg)", options))
    add_tag(selectInput(ns("bmi"), "BMI (kg/m^2)", options))
    add_tag(selectInput(ns("sitht"), "Sitting height (cm)", options))
    add_tag(selectInput(ns("leglen"), "Leg length (cm)", options))
    
    add_tag(selectInput(ns("to_add"), "Calculate", c("SDS", "Centile", "% Predicted", "Predicted", "% CV", "Skewness"), selected = "SDS", multiple = TRUE, selectize = TRUE))
    add_tag(actionButton(ns('apply'), 'Apply'))
    add_tag(downloadButton(ns("download_data"), "Download"))
    
    output_tags
  })
  
  # when the 'apply' button is clicked
  observeEvent(input$apply, {
    df <- isolate(original_data$df)
    # loop over every measurement 
    for (item in measurement_names) {
      # do the calculation for the measurement
      df <- get_observer(item$name, df)
    }
    original_data$df <- df
  })
  
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
  
  get_age <- function() {
      df <- isolate(original_data$df)
      age_column_or_value <- isolate(input$age_source)
      age_column = df[, strsplit(age_column_or_value, split=" ")[[1]][[2]]]
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
  
  get_observer <- function(measurement_name, df) {
      value <- input[[measurement_name]]
      input_name <- measurement_names[[measurement_name]]$name
      code_name <- measurement_names[[measurement_name]]$code
      
      new_col <- function(stat, column, code_name) {
        paste('LMS', stat, column, code_name, sep = '_')
      }
      
      if (!is.null(value) && value != 'N/A') {
        column = strsplit(value, split=" ")[[1]][2]
        sex_column_or_value <- get_sex()
        age_column_or_value <- get_age() 
        # TODO: convert age into year units
        
        lms_stats <- .measurement_to_scores(age_column_or_value, sex_column_or_value, code_name, df[, column])
        
        if ('SDS' %in% input$to_add) {
          df[[new_col('SDS', column, code_name)]] <- .get_sds(lms_stats)
        }
        
        if ('Centile' %in% input$to_add) {
          df[[new_col('Centile', column, code_name)]] <- .get_centile(lms_stats)
        }
        
        if ('% Predicted' %in% input$to_add) {
          df[[new_col('PercPredicted', column, code_name)]] <- .get_perc_predicted(lms_stats)
        }
        
        if ('Predicted' %in% input$to_add) {
          df[[new_col('Predicted', column, code_name)]] <- .get_predicted(lms_stats)
        }
        
        if ('% CV' %in% input$to_add) {
          df[[new_col('PercCV', column, code_name)]] <- .get_perc_cv(lms_stats)
        }
        
        if ('Skewness' %in% input$to_add) {
          df[[new_col('Skewness', column, code_name)]] <- .get_skewness(lms_stats)
        }
        
        # fix the column order if it didn't exist already
        # if (is.na(match(new_column, columns))) {
        #   new_order <- append(columns, new_column, which(columns==column))  
        #   df <- df[new_order]
        # }
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

