library(shiny)

source('R/calculator.R', local=TRUE)
source('R/example.R', local=TRUE)

# Main UI for the tabs ########################################################
.ui <- navbarPage("LMSgrowth2",
                 tabPanel("Calculator", .calculatorUI("calculator")),
                 tabPanel("Next", bootstrapPage("TODO")),
                 tabPanel("Next", bootstrapPage("TODO")),
                 tabPanel("Example", .exampleUI("example"))
)

# Shiny server logic ##########################################################
.server <- function(input, output) {
  callModule(.calculator, "calculator", stringAsFactors=FALSE)
  callModule(.example, "example", stringAsFactors=FALSE)
}

launchApp <- function() {
  shinyApp(ui = .ui, server = .server)
}

