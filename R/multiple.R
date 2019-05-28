# An example tab ##############################################################
.multipleUI <- function(id, label="multiple measurements") {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Measurements to SDS"),

        uiOutput(ns("fileInput")),
        
        # after uploading data, we show:
        uiOutput(ns("sexSelect")),
        uiOutput(ns("ageSelect")),
        uiOutput(ns("measureSelect"))
        ),
      
      mainPanel(
        div(style='height:600px; overflow-y: scroll',
            tableOutput(ns("table"))
        ),
        textOutput(ns("raw_output"))
      )
      )
      
    )
}

# Example server logic ########################################################
.multiple <- function(input, output, session, stringAsFactors) {
  # To use `renderUI` within modules, we need to wrap with `ns()`
  ns <- session$ns
  
  rvalues <- reactiveValues(
    df = NULL
  )
  
  cols <- reactiveVal(NULL)
  
  # upload data and set dataframe
  observe({
    if(!is.null(input$file)) {
      df <- as.data.frame(read.csv(input$file$datapath))
      cols(names(df))
      rvalues$df <- df
    }
  })
  
  # hide the file input once rvalue has been updated
  output$fileInput <- renderUI({
    if(is.null(rvalues$df)) {
      return(fileInput(ns('file'), 'Upload data file'))
    } else {
      return(NULL)
    }
  })
  
  output$table <- renderTable(rvalues$df)
  
  # Select variables:
  output$sexSelect <- renderUI({
    if (is.null(rvalues$df)) return(NULL)
    match <- intersect(c('Sex', 'sex', 'Gender', 'gender'), isolate(cols()))
    if (length(match) == 1) {
      selected <- paste('[Column] ', match, sep='')
    } else {
      selected <- 'Male'
    }
    
    options <-  c('Male', 'Female', paste('[Column] ', isolate(cols()), sep=''))
    
    # Variable selection:    
    selectInput(ns("sex"), 
                label = "Sex", 
                choices = options, 
                selected = selected, multiple =FALSE)
  })
  
  output$ageSelect <- renderUI({
    # only display age selection if we have a dataframe
    if (is.null(rvalues$df)) return(NULL)
    match <- intersect(c('Age', 'age', 'Years', 'years', 'Days', 'days'), isolate(cols()))
    
    if (length(match) == 1) {
      selected <- paste('[Column] ', match, sep='')
    } else if (length(match) > 1) {
      selected <- paste('[Column] ', match[1], sep='')
    } else {
      selected <- ''
    }
    
    options <-  paste('[Column] ', isolate(cols()), sep='')
    
    output = tagList()
    output[[1]] = selectInput(ns("ageSource"), "Age", options, selected = selected)
    output[[2]] = selectInput(ns("ageUnit"), "Unit of age", c('Days', 'Weeks', 'Months', 'Years'), selected = 'years')
    
    output
    
  })
  
  output$measureSelect <- renderUI({
    # only display measurement selection if we have loaded a dataframe
    if (is.null(rvalues$df)) return(NULL)
    
    options <-  c('N/A', paste('[Column] ', isolate(cols()), sep=''))
    output = tagList()
    output[[1]] = selectInput(ns("height"), "Height (cm)", options)
    output[[2]] = selectInput(ns("weight"), "Weight (kg)", options)
    output[[3]] = selectInput(ns("bmi"), "BMI (kg/m^2)", options)
    output[[4]] = selectInput(ns("sitht"), "Sitting height (cm)", options)
    output[[5]] = selectInput(ns("legln"), "Leg length (cm)", options)
    
    output[[6]] = fluidPage(actionButton(ns('Reset'), 'Reset'), actionButton(ns('Apply'), 'Apply') )
    output
  })
  
  get_sex <- function() {
    sex_selected <- isolate(input$sex)
    if (startsWith(sex_selected, '[Column]')) {
      df <- isolate(rvalues$df)
      sex_column_or_value = df[, strsplit(sex_selected, split=" ")[[1]][2]]
    } else {
      sex_column_or_value = sex_selected
    }
    return(sex_column_or_value)
  }
  
  get_age <- function() {
      df <- isolate(rvalues$df)
      age_column_or_value <- isolate(input$ageSource)
      age_column = df[, strsplit(age_column_or_value, split=" ")[[1]][[2]]]
      return(age_column)
  }
  
  measurement_names <- list(
    height=list(name='height', code='ht'),
    weight=list(name='weight', code='wt')
  )
  
  get_observer <- function(measurement_name) {
      value <- input[[measurement_name]]
      input_name <- measurement_names[[measurement_name]]$name
      code_name <- measurement_names[[measurement_name]]$code
      
      if (!is.null(value) && value != 'N/A') {
        df <- isolate(rvalues$df)
        column = strsplit(value, split=" ")[[1]][2]
        sex_column_or_value <- get_sex()
        age_column_or_value <- get_age()
        
        lms_stats <- .measurement_to_scores(age_column_or_value, sex_column_or_value, code_name, df[, column])
        new_column = paste(column, '@', code_name, '_SDS', sep='')
        columns <- names(df)
        df[[new_column]] <- lms_stats$z
        
        # fix the column order if it didn't exist already
        if (is.na(match(new_column, columns))) {
          new_order <- append(columns, new_column, which(columns==column))  
          df <- df[new_order]
        }
        
        rvalues$df <- df
    }
  }
  
  observeEvent(input$height, get_observer('height'))
  observeEvent(input$weight, get_observer('weight'))
}

