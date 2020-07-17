#' @include functions.R single.R multiple.R centile.R density.R help.R preferences.R 
#' @import shiny
#' @importFrom dplyr %>%
NULL

# This will supress the devtools check note "no visible binding for global variable"
# https://stackoverflow.com/questions/23475309/in-r-is-it-possible-to-suppress-note-no-visible-binding-for-global-variable
utils::globalVariables('.')

#-------------------------------------------------------------------------------
# Main UI and server components for the LMSgrowth2 Shiny application. We're using 
# Shiny modules: see https://shiny.rstudio.com/articles/modules.html
# 
# The main UI is a collection of tab panels. Each module is loaded as a separate 
# tab panel.
#-------------------------------------------------------------------------------

.ui <- function() {
  shiny::addResourcePath("LMSgrowth2", system.file('assets', package='LMSgrowth2'))
  tagList(
    tags$script(src="LMSgrowth2/js.cookie.js"),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script=system.file('assets', 'shinyjs.js', package='LMSgrowth2')),
    navbarPage(
      "LMSgrowth2",
      tabPanel("One child", .singleUI("single")),
      tabPanel("Multiple children", .multipleUI("multiple")),
      tabPanel("Centiles", .centileUI("centile")),
      tabPanel("LMS density", .densityUI("density")),
      tabPanel("Help", .helpUI("help")),
      tabPanel("Preferences", .preferencesUI("preferences"))
    )
  )
}

.server <- function(input, output, session) {
  shinyhelper::observe_helpers(help_dir=system.file('assets', package='LMSgrowth2'))
  
  # The preferences module returns a reactive value holding user preferences used in
  # all modules. That reactive value is passed as argument `globals` to all modules
  globals <- callModule(.preferences, "preferences")
  
  callModule(.single, "single", globals = globals)
  callModule(.multiple, "multiple", globals = globals)
  callModule(.centile, "centile", globals = globals)
  callModule(.density, "density", globals = globals)
  callModule(.help, "help", globals = globals)
}
