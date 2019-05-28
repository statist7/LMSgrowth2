# An example tab ##############################################################
.multipleUI <- function(id, label="multiple measurements") {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Measurements to SDS"),

        uiOutput(ns("fileInput")),
        
        # after uploading data, we show:
        uiOutput(ns("measurementForm"))
        ),
      
      mainPanel(
        div(style='height:600px; overflow-y: scroll', tableOutput(ns("table")))
      )
    )
  )
}

# Example server logic ########################################################
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
  output$fileInput <- renderUI({
    if(is.null(original_data$df)) {
      return(fileInput(ns('file'), 'Upload data file'))
    } else {
      return(NULL)
    }
  })
  
  output$table <- renderTable(original_data$df)
  
  # display the input form, populating the options from the uploaded data
  output$measurementForm <- renderUI({
    if (!original_data$initialised) return(NULL)
    
    o <- tagList()
    
    # to increment index when adding tags
    n <- function(item) {
      return(length(item) + 1)
    }
    
    # select input for sex
    match <- intersect(c('Sex', 'sex', 'Gender', 'gender'), isolate(original_columns()))
    if (length(match) == 1) {
      selected <- paste('[Column] ', match, sep='')
    } else {
      selected <- 'Male'
    }
    
    options <-  c('Male', 'Female', paste('[Column] ', isolate(original_columns()), sep=''))
    
    # Variable selection
    o[[n(o)]] = selectInput(ns("sex"),  label = "Sex",  choices = options,  selected = selected, multiple =FALSE)
    
    match <- intersect(c('Age', 'age', 'Years', 'years', 'Days', 'days'), isolate(original_columns()))
    
    if (length(match) == 1) {
      selected <- paste('[Column] ', match, sep='')
    } else if (length(match) > 1) {
      selected <- paste('[Column] ', match[1], sep='')
    } else {
      selected <- ''
    }
    
    options <-  paste('[Column] ', isolate(original_columns()), sep='')
    
    o[[n(o)]] = selectInput(ns("ageSource"), "Age", options, selected = selected)
    o[[n(o)]] = selectInput(ns("ageUnit"), "Unit of age", c('Days', 'Weeks', 'Months', 'Years'), selected = 'years')
    
    # only display measurement selection if we have loaded a dataframe
    options <-  c('N/A', paste('[Column] ', isolate(original_columns()), sep=''))
    o[[n(o)]] = selectInput(ns("height"), "Height (cm)", options)
    o[[n(o)]] = selectInput(ns("weight"), "Weight (kg)", options)
    o[[n(o)]] = selectInput(ns("bmi"), "BMI (kg/m^2)", options)
    o[[n(o)]] = selectInput(ns("sitht"), "Sitting height (cm)", options)
    o[[n(o)]] = selectInput(ns("legln"), "Leg length (cm)", options)
    
    o[[n(o)]] = selectInput(ns("to_add"), "Calculate", c("SDS", "Centile", "% Predicted", "Predicted", "% CV", "Skewness"), selected = "SDS", multiple = TRUE, selectize = TRUE)
    o[[n(o)]] = actionButton(ns('apply'), 'Apply')
    # output[[nextidx(output)]] = fluidPage(actionButton(ns('Reset'), 'Reset'), actionButton(ns('Apply'), 'Apply') )
    o[[n(o)]] = downloadButton(ns("downloadData"), "Download")
    
    o
  })
  
  observeEvent(input$apply, {
    df <- isolate(original_data$df)
    for (item in measurement_names) {
      print(item)
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
      age_column_or_value <- isolate(input$ageSource)
      age_column = df[, strsplit(age_column_or_value, split=" ")[[1]][[2]]]
      return(age_column)
  }
  
  # list of possible calculations available
  # TODO: populate from selected reference sheet
  measurement_names <- list(
    height=list(name='height', code='ht'),
    weight=list(name='weight', code='wt')
  )
  
  get_observer <- function(measurement_name, df) {
      value <- input[[measurement_name]]
      input_name <- measurement_names[[measurement_name]]$name
      code_name <- measurement_names[[measurement_name]]$code
      
      if (!is.null(value) && value != 'N/A') {
        column = strsplit(value, split=" ")[[1]][2]
        sex_column_or_value <- get_sex()
        age_column_or_value <- get_age()
        
        lms_stats <- .measurement_to_scores(age_column_or_value, sex_column_or_value, code_name, df[, column])
        
        # centile <- sitar::z2cent(sds)
        # perc_predicted <- 100 * lms_stats$value / lms_stats$M
        # predicted <- lms_stats$M
        # perc_cv <- lms_stats$S * 100
        # skewness <- lms_stats$L
        
        if ('SDS' %in% input$to_add) {
          new_column = paste('LMS', 'SDS', column, code_name, sep='_')
          df[[new_column]] <- lms_stats$z
        }
        
        if ('Centile' %in% input$to_add) {
          new_column = paste('LMS', 'Centile', column, code_name, sep='_')
          df[[new_column]] <- sitar::z2cent(lms_stats$z)
        }
        
        if ('% Predicted' %in% input$to_add) {
          new_column = paste('LMS', 'PercPredicted', column, code_name, sep='_')
          df[[new_column]] <- 100 * lms_stats$value / lms_stats$M
        }
        
        if ('Predicted' %in% input$to_add) {
          new_column = paste('LMS', 'Predicted', column, code_name, sep='_')
          df[[new_column]] <- lms_stats$M
        }
        
        if ('% CV' %in% input$to_add) {
          new_column = paste('LMS', 'PercCV', column, code_name, sep='_')
          df[[new_column]] <- lms_stats$S * 100
        }
        
        if ('Skewness' %in% input$to_add) {
          new_column = paste('LMS', 'Skewness', column, code_name, sep='_')
          df[[new_column]] <- lms_stats$L
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
  
  output$downloadData <- downloadHandler(
    filename = 'lms_download.csv',
    content = function(file) {
      write.csv(original_data$df, file, row.names = FALSE)
    }
  )
  
}

