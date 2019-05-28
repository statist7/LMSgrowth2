library(shiny)

source('R/calculator.R', local=TRUE)
source('R/example.R', local=TRUE)
source('R/multiple.R', local=TRUE)

# Main UI for the tabs ########################################################
.ui <-tagList(
    navbarPage(
      "LMSgrowth2",
      tabPanel("Calculator", .calculatorUI("calculator")),
      tabPanel("Multiple", .multipleUI("multiple")),
      tabPanel("Next", bootstrapPage("TODO")),
      tabPanel("Next", bootstrapPage("TODO")),
      tabPanel("Example", .exampleUI("example"))
    )  
  )
  

# Shiny server logic ##########################################################
.server <- function(input, output) {
  callModule(.calculator, "calculator", stringAsFactors=FALSE)
  callModule(.multiple, "multiple", stringAsFactors=FALSE)
  callModule(.example, "example", stringAsFactors=FALSE)
}

launchApp <- function() {
  shinyApp(ui = .ui, server = .server)
}
