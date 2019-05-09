# An example tab ##############################################################
exampleUI <- function(id, label="example ui") {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Example"),
        textInput(ns("name"), "name", value="world")
      ),
      mainPanel(
        textOutput(ns("greeting"))
      )
    )
  )
}

# Example server logic ########################################################
example <- function(input, output, session, stringAsFactors) {
  output$greeting <- renderText({
    paste('Hello,', input$name)
  })
}
