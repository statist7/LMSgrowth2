# An example tab ##############################################################
.multipleUI <- function(id, label="multiple measurements") {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Measurements to SDS"),
        fileInput(ns("file"), "Upload data-file:"),
        
        # Variable selection:
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
  ns <- NS('multiple')
  
  rvalues <- reactiveValues(
    df = NULL
  )
  
  cols <- reactiveVal(NULL)
  
  observe({
    if(!is.null(input$file)) {
      df <- as.data.frame(read.csv(input$file$datapath))
      cols(names(df))
      rvalues$df <- df
    }
  })
  
  output$table <- renderTable(rvalues$df)
  
  # Select variables:
  output$sexSelect <- renderUI({
    
    if (is.null(isolate(rvalues$df)) && is.null(input$file)) return(NULL)
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
    
    if (is.null(isolate(rvalues$df)) && is.null(input$file)) return(NULL)
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
    if (is.null(isolate(rvalues$df)) && is.null(input$file)) return(NULL)
    options <-  c('N/A', paste('[Column] ', isolate(cols()), sep=''))
    output = tagList()
    output[[1]] = selectInput(ns("height"), "Height (cm)", options)
    output[[2]] = selectInput(ns("weight"), "Weight (kg)", options)
    output[[3]] = selectInput(ns("bmi"), "BMI (kg/m^2)", options)
    output[[4]] = selectInput(ns("sitht"), "Sitting height (cm)", options)
    output[[5]] = selectInput(ns("legln"), "Leg length (cm)", options)
    output
  })
  
  observe({
    height_value <- input$height
    if (!is.null(height_value) && height_value != 'N/A') {
      df <- isolate(rvalues$df)
      column = strsplit(height_value, split=" ")[[1]][2]
      sex_value <- isolate(input$sex)
      if (startsWith(sex_value, '[Column]')) {
        sex_column = df[, strsplit(sex_value, split=" ")[[1]][2]]
      } else {
        sex_column = sex_value
      }
      age_value <- isolate(input$ageSource)
      age_column = df[, strsplit(age_value, split=" ")[[1]][[2]]]
      
      lms_stats <- .measurement_to_scores(age_column, sex_column, 'ht', df[, column])
      df$height_sds <- lms_stats$z
      rvalues$df <- df
    }
  })
}

