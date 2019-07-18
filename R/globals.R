require(shinyjs)

# The globals shiny module is used to store global state for the application
# Reading/writing of cookies should be done here to be picked up by other modules
# in the application

.globalsUI <- function(id, label="globals ui") {
  ns <- NS(id)
  
  fluidPage(
    textInput(ns("globalValue1"), "globalValue1", value="testing"),
    selectInput(ns("col"), "Colour:",
                c("white", "yellow", "red", "blue", "purple"), selected="white"),
    textOutput(ns("testout"))
  )
}

.globals <- function(input, output, session) {
  
  stash <- reactiveValues()
  
  observeEvent(input$globalValue1, {
    stash$globalValue1 <- input$globalValue1
  })
  
  observeEvent(input$col, {
    js$pageCol(input$col)
    stash$globalValue2 <- input$col
  })
  
  setGlobalValue3 <- function(value) {
    stash$globalValue3 <- value
  }
  
  output$testout <- renderText({
    stash$globalValue3
  })
  
  out <- list(
    getGlobalValue1 = reactive(stash$globalValue1),
    getGlobalValue2 = reactive(stash$globalValue2),
    getGlobalValue3 = reactive(stash$globalValue3),
    setGlobalValue3 = setGlobalValue3
    )
  return(out)
}

