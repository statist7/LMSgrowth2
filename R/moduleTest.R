.moduleTestUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    h1("Tests to ensure shinyjs works in Shiny modules"),
    br(),
    h4("Clicking on the Name text input will show an alert box after 1 second"),
    div(id = ns('form'),
        textInput(ns('text'), 'Name', value = "Dean"),
        dateInput(ns('date'), 'Date', format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en"),
        actionButton(ns("resetForm"), "Reset form"),
        actionButton(ns("resetText"), "Reset text"),
        actionButton(ns("hideForm"), "Hide form"),
        actionButton(ns("toggleText"), "Toggle text"),
        actionButton(ns("disableText"), "Disable text"),
        actionButton(ns("enableText"), "Enable text")
    )
  )
}

.moduleTest <- function(input, output, session, stringAsFactors) {
  shinyjs::onclick("text", shinyjs::delay(1000, shinyjs::info("clicked me!")))
  shinyjs::onevent("mouseenter", "text", shinyjs::logjs("entered"))
  observeEvent(input$resetForm, shinyjs::reset("form"))
  observeEvent(input$resetText, shinyjs::reset("text"))
  observeEvent(input$hideForm, shinyjs::hide("form"))
  observeEvent(input$toggleText, shinyjs::toggle("text", anim = TRUE))
  observeEvent(input$disableText, shinyjs::disable("text"))
  observeEvent(input$enableText, shinyjs::enable("text"))
}
