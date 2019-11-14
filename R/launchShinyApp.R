library(shiny)
library(shinyjs)
library(purrr)
library(stringr)
library(plotly)
library(DT)
library(formattable)

source('R/calculator.R', local=TRUE)
source('R/example.R', local=TRUE)
source('R/multiple.R', local=TRUE)
source('R/centile.R', local=TRUE)
source('R/density.R', local=TRUE)
source('R/globals.R', local=TRUE)



# Load Javascript functions because can't get 
# the extendShinyjs(script='...') working in a package
source('R/javascript.R', local=TRUE)

# Main UI for the tabs ########################################################
.ui <- tagList(
  tags$head(tags$script(src="js.cookie.js")),
  useShinyjs(),
  extendShinyjs(text = .jsCode, functions = .jsCodeFunctions),
  navbarPage(
    "LMSgrowth2",
    tabPanel("Calculator", .calculatorUI("calculator")),
    tabPanel("Multiple", .multipleUI("multiple")),
    tabPanel("Centile", .centileUI("centile")),
    tabPanel("Example", .exampleUI("example")),
    tabPanel("Density", .densityUI("density")),
    tabPanel("Preferences", .globalsUI("globals"))
  )
)

# Shiny server logic ##########################################################
.server <- function(input, output, session) {
  globals <- callModule(.globals, "globals")
  callModule(.calculator, "calculator", globals = globals)
  callModule(.multiple, "multiple", globals = globals)
  callModule(.centile, "centile", globals = globals)
  callModule(.example, "example", globals = globals)
  callModule(.density, "density", globals = globals)
}

launchApp <- function() {
  loadjs <- function() {
    require(shinyjs)
    useShinyjs()
    extendShinyjs(text = .jsCode, functions = .jsCodeFunctions)
  }
  shinyApp(ui = .ui, server = .server, onStart = loadjs)
}

runPackage <- function() {
  shiny::runApp(system.file('shinyApp', package='LMSgrowth2'))
}
