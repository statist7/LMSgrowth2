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
  
  dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      print('input$file is null')
      return(data.frame())
    }
    
    print('input$file is not null')
    dataset <- as.data.frame(read.csv(input$file$datapath))
    print('loaded dataset')
    return(dataset)
  })
  
  output$table <- renderTable({
    return(dataset())
  })
  
  # Select variables:
  output$sexSelect <- renderUI({
    
    if (identical(dataset(), '') || identical(dataset(),data.frame())) return(NULL)
    
    match <- intersect(c('Sex', 'sex', 'Gender', 'gender'), names(dataset()))
    if (length(match) == 1) {
      selected <- paste('[Column] ', match, sep='')
    } else {
      selected <- 'Male'
    }
    
    options <-  c('Male', 'Female', paste('[Column] ', names(dataset()), sep=''))
    
    # Variable selection:    
    selectInput(ns("sex"), 
                label = "Sex", 
                choices = options, 
                selected = selected, multiple =FALSE)
  })
  
  output$ageSelect <- renderUI({
    if (identical(dataset(), '') || identical(dataset(),data.frame())) return(NULL)
  
    match <- intersect(c('Age', 'age', 'Years', 'years', 'Days', 'days'), names(dataset()))
    
    if (length(match) == 1) {
      selected <- paste('[Column] ', match, sep='')
    } else if (length(match) > 1) {
      selected <- paste('[Column] ', match[1], sep='')
    } else {
      selected <- ''
    }
    
    options <-  paste('[Column] ', names(dataset()), sep='')
    
    output = tagList()
    output[[1]] = selectInput(ns("ageSource"), "Age", options, selected = selected)
    output[[2]] = selectInput(ns("ageUnit"), "Unit of age", c('Days', 'Weeks', 'Months', 'Years'), selected = 'years')
    
    output
    
  })
  
  output$measureSelect <- renderUI({
    if (identical(dataset(), '') || identical(dataset(),data.frame())) return(NULL)
    options <-  c('N/A', paste('[Column] ', names(dataset()), sep=''))
    output = tagList()
    output[[1]] = selectInput(ns("height"), "Height (cm)", options)
    output[[2]] = selectInput(ns("weight"), "Weight (kg)", options)
    output[[3]] = selectInput(ns("bmi"), "BMI (kg/m^2)", options)
    output[[4]] = selectInput(ns("sitht"), "Sitting height (cm)", options)
    output[[5]] = selectInput(ns("legln"), "Leg length (cm)", options)
    output
  })
  
  heightSDS <- reactive({
    if (identical(dataset(), '') || identical(dataset(),data.frame())) return(NULL)
    if (is.null(input$height) || input$height == 'N/A') return(NULL)
    column = strsplit(input$height, split=" ")[[1]][2]
    dataset()[, column]
  })
  
  output$raw_output <- reactive({
    if (!is.null(heightSDS())) {
      if (startsWith(input$sex, '[Column]')) {
        sex_column = dataset()[, strsplit(input$sex, split=" ")[[1]][2]]
      } else {
        sex_column = input$sex
      }
      
      age_column = dataset()[, strsplit(input$ageSource, split=" ")[[1]][[2]]]
      
      lms_stats <- .measurement_to_scores(age_column, sex_column, 'ht', heightSDS())
      paste(lms_stats$z[1:10], collapse =" ")
    } else {
      return(NULL)
    }
  })
  
}

