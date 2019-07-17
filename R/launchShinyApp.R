library(shiny)
library(shinyjs)


source('R/calculator.R', local=TRUE)
source('R/example.R', local=TRUE)
source('R/multiple.R', local=TRUE)
source('R/lmsplot.R', local=TRUE)
source('R/user_session.R', local=TRUE)

# Cookie handling
jsCode <- '
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

# Main UI for the tabs ########################################################
.ui <- tagList(
    tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.2.0/js.cookie.js")),
    useShinyjs(),
    extendShinyjs(text = jsCode, functions=c('getcookies', 'setcookie', 'rmcookie', 'rmcookies')),
    navbarPage(
      "LMSgrowth2",
      tabPanel("Calculator", .calculatorUI("calculator")),
      tabPanel("Multiple", .multipleUI("multiple")),
      tabPanel("LMS dist", .lmsplotUI("lmsplot")),
      tabPanel("Next", bootstrapPage("TODO")),
      tabPanel("Next", bootstrapPage("TODO")),
      tabPanel("Example", .exampleUI("example")),
      tabPanel("User session", .usersessionUI("user_session"))
    )
  )

# Shiny server logic ##########################################################
.server <- function(input, output) {
  # javascript cookie handling, used throughout
  status <- reactiveVal(value = NULL)
  
  observe({
    js$getcookies()
    status(input$jscookie)
  })
  
  output$output <- renderPrint({
    status()
  })
  
  observeEvent(input$login, {
    js$setcookie(name=stringi::stri_rand_strings(1, 5, pattern='[A-Za-z]'), value=runif(1))
  })
  
  observeEvent(input$logout, {
    js$rmcookies()
  })
  
  callModule(.calculator, "calculator", stringAsFactors=FALSE)
  callModule(.multiple, "multiple", stringAsFactors=FALSE)
  callModule(.lmsplot, "lmsplot", stringAsFactors=FALSE)
  callModule(.example, "example", stringAsFactors=FALSE)
  callModule(.usersession, "user_session", stringAsFactors=FALSE)
}

launchApp <- function() {
  
  shinyApp(ui = .ui, server = .server)
}
