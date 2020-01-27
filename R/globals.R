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
  # globalValues$roundToDigits (numeric)
  #     - number of decimal digits for rounding
  # globalValues$roundToSignificantDigits (numeric)
  #     - number of significant digits for rounding
  # globalValues$growthReference (string)
  #     - the currently selected growth reference e.g. "uk90"
  # globalValues$growthReferenceName (string)
  #     - description of selected growth reference e.g. "UK 1990 growth reference"
  # globalValues$growthReferenceMeasures (character vector)
  #     - code of measures available in currently selected growth reference
  # growthReferenceAgeStart (numeric)
  #     - the first age available in currently selected growth reference
  # growthReferenceAgeStop (numeric)
  #     - the last age available in currently selected growth reference
  # growthReferenceSexes (string)
  #     - the list of sexes available in currently selected growth reference
  globalValues <- reactiveValues(roundToDigits = 2,
                                 roundToSignificantDigits = 3,
                                 # Use the same default as `ncentiles` and
                                 # `centilestep` in `centile.R`.
                                 z_scores = .get_sds_range(7, 0.666))

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
    references <- .get_references()
    globalValues$growthReferenceName <- names(references)[references == globalValues$growthReference]
    sitar_data <- .get_sitar_data(input$growth_ref)
    globalValues$growthReferenceMeasures <- .get_measures_for_data(sitar_data)
    ages <- .get_ages_for_data(sitar_data)
    globalValues$growthReferenceAgeStart <- ages[1]
    globalValues$growthReferenceAgeStop <- ages[length(ages)]
    globalValues$growthReferenceSexes <- .get_sexes_for_data(sitar_data)
  })

  return(globalValues)
}
