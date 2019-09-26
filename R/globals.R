require(shinyjs)

source('R/functions.R', local = TRUE)

# The globals shiny module is used to store global state for the application
# Reading/writing of cookies should be done here to be picked up by other modules
# in the application

.globalsUI <- function(id, label="globals ui") {
  ns <- NS(id)
  
  fluidPage(
    selectInput(ns("growth_ref"), label = "Growth reference", choices = .get_references())
  )
}

.globals <- function(input, output, session) {
  # store the page load status
  status <- reactiveValues(loaded=FALSE)
  
  # object to store global values
  # globalValues$growthReference (string) 
  #     - the currently selected growth reference e.g. "uk90"
  # globalValues$growthReferenceMeasures (character vector) 
  #     - code of measures available in currently selected growth reference
  globalValues <- reactiveValues()
  
  # this block only executed once on page load to apply saved cookie state, if any
  observe({
    if (!status$loaded & !is.null(input$jscookie[["growthRef"]])) {
      status$loaded <- TRUE
      saved_ref <- isolate(input$jscookie$growthRef)
      updateSelectInput(session, "growth_ref", label="Growth reference", selected = saved_ref)
    }
  })

  # if user selects growth_ref, then update the cookie
  observeEvent(input$growth_ref, {
    js$setcookie(name='growthRef', value=input$growth_ref)
    globalValues$growthReference <- input$growth_ref
    globalValues$growthReferenceMeasures <- .get_measures_for_ref(input$growth_ref)
  })
  
  return(globalValues)
}

