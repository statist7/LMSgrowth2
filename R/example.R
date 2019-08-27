# An example tab ##############################################################
.exampleUI <- function(id, label="example ui") {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Example"),
        textInput(ns("name"), "name", value="world"),
        selectInput(ns('dataset'), 'Select Dataset', 
                    list(iris = "iris",
                         mtcars = "mtcars")),
        
        conditionalPanel(
          condition = "output['example-factorflag'] == true",
          checkboxInput(ns("UseFactor"), "Add Factor Variable")
        ) 
      ),
      mainPanel(
        textOutput(ns("greeting"))
      )
    )
  )
}

# Example server logic ########################################################
.example <- function(input, output, session, globals) {
  ns <- session$ns
  
  output$greeting <- renderText({
    paste('Hello,', input$name)
  })
  
  df <- reactive({
    if(input$dataset == "iris"){
      data("iris")
      iris
    }else {
      data("mtcars")
      mtcars
    }
  })
  output$factorflag <- reactive("factor" %in% sapply(df(),class))
  outputOptions(output, "factorflag", suspendWhenHidden = FALSE)
}
