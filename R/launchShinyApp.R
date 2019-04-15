# create the shiny application user interface
shinyAppUI <- fluidPage(

  # Application title
  titlePanel("LMSgrowth2"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Get height SDS for 4 year-old girl"),
      numericInput("height_id", "Height (cm)", value=70, min = 5, max = 300)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("sds_output")
    )
  )
)

# Define server logic required to draw a histogram
shinyAppServer <- function(input, output) {
  output$sds_output <- renderText({
    paste("SDS is", get_height_sds(4, input$height_id, 2))
  })
}

launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}

