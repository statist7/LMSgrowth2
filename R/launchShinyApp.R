library(shiny)
library(shinyjs)


source('R/calculator.R', local=TRUE)
source('R/example.R', local=TRUE)
source('R/multiple.R', local=TRUE)
source('R/lmsplot.R', local=TRUE)

# Cookie handling
jsCode <- '
  shinyjs.pageCol = function(params) {
    $("body").css("background", params);
  }
  
  shinyjs.getcookies = function(params) {
    Shiny.setInputValue("jscookie", Cookies.get(), {priority: "event"});
  }

  shinyjs.setcookie = function(params) {
    Cookies.set(params.name, escape(params.value));  
    Shiny.setInputValue("jscookie", Cookies.get(), {priority: "event"});
  }

  shinyjs.rmcookie = function(name) {
    Cookies.remove(name);
    Shiny.setInputValue("jscookie", Cookies.get(), {priority: "event"});
  }
  
  shinyjs.rmcookies = function(params) {
    for (var key in Cookies.get()) {
      Cookies.remove(key);
    }
    Shiny.setInputValue("jscookie", Cookies.get(), {priority: "event"})
  }
'
jsCodeFunctions <- c('getcookies', 'setcookie', 'rmcookie', 'rmcookies', 'pageCol')

# Main UI for the tabs ########################################################
.ui <- tagList(
  tags$head(tags$script(src="js.cookie.js")),
  useShinyjs(),
  extendShinyjs(text = jsCode, functions=jsCodeFunctions),
  selectInput("col", "Colour:",
              c("white", "yellow", "red", "blue", "purple")),
  navbarPage(
    "LMSgrowth2",
    tabPanel("Calculator", .calculatorUI("calculator")),
    tabPanel("Multiple", .multipleUI("multiple")),
    tabPanel("LMS dist", .lmsplotUI("lmsplot")),
    tabPanel("Next", bootstrapPage("TODO")),
    tabPanel("Next", bootstrapPage("TODO")),
    tabPanel("Example", .exampleUI("example"))
  )
)

# Shiny server logic ##########################################################
.server <- function(input, output) {
  callModule(.calculator, "calculator", stringAsFactors=FALSE)
  callModule(.multiple, "multiple", stringAsFactors=FALSE)
  callModule(.lmsplot, "lmsplot", stringAsFactors=FALSE)
  callModule(.example, "example", stringAsFactors=FALSE)
  
  # javascript cookie handling, used throughout
  status <- reactiveVal(value = NULL)
  observeEvent(input$col, {
    js$pageCol(input$col)
  })
}

launchApp <- function() {
  loadjs <- function() {
    require(shinyjs)
    useShinyjs()
    extendShinyjs(text = jsCode, functions=jsCodeFunctions)
  }
  
  shinyApp(ui = .ui, server = .server, onStart = loadjs)
}


runPackage <- function() {
  shiny::runApp(system.file('shinyApp', package='LMSgrowth2'))
}
