require(shinyjs)

source('R/functions.R', local = TRUE)

# The globals shiny module is used to store global state for the application
# Reading/writing of cookies should be done here to be picked up by other modules
# in the application

.globalsUI <- function(id, label="globals ui") {
  ns <- NS(id)
  
  fluidPage(
    selectInput(ns("growth_ref"), label = "Growth reference", choices=c())
  )
}

.globals <- function(input, output, session) {
  observe({
    refs <- .get_references()
    updateSelectInput(session, "growth_ref", label="Growth reference", choices=refs, selected = 'uk90')
  })
  
  stash <- reactiveValues()
  
  observeEvent(input$growth_ref, {
    stash$growthReference <- input$growth_ref
  })
  
  # setGlobalValue3 <- function(value) {
  #   stash$globalValue3 <- value
  # }
  
  out <- list(
    getGrowthReference = reactive(stash$growthReference)
    # getGlobalValue3 = reactive(stash$globalValue3),
    # setGlobalValue3 = setGlobalValue3
    )
  return(out)
}

