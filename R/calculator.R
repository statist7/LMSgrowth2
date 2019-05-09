source('R/functions.R', local = TRUE)

# Measurement to SDS calculator ###############################################
calculatorUI <- function(id, label="calculator ui") {
  ns <- NS(id)
  
  fluidPage(
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        h3("Measurement to SDS"),
        radioButtons(ns("sex"), label = h4("Sex"),
                     choices = list("Male" = 1, "Female" = 2), 
                     selected = 1),
        h4("Age"),
        numericInput(ns("age_years"), "years", value=0, min=0),
        numericInput(ns("age_months"), "months", value=0, min=0),
        numericInput(ns("age_weeks"), "weeks", value=0, min=0),
        numericInput(ns("age_days"), "days", value=0, min=0),
        h4("Measurements"),
        numericInput(ns("height"), "Height (cm)", value="", min = 0, max = 300),
        numericInput(ns("weight"), "Weight (kg)", value="", min = 0, max = 300),
        numericInput(ns("bmi"), "BMI (kg/m^2)", value="", min = 0, max = 300),
        numericInput(ns("sitht"), "Sitting height (cm)", value="", min = 0, max = 300),
        numericInput(ns("legln"), "Leg length (cm)", value="", min = 0, max = 300)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        p("(Using British1990 reference)"),
        verbatimTextOutput(ns("age_info")),
        verbatimTextOutput(ns("height_info")),
        verbatimTextOutput(ns("weight_info")),
        verbatimTextOutput(ns("bmi_info")),
        verbatimTextOutput(ns("sitht_info")),
        verbatimTextOutput(ns("leglen_info"))
      )
    )
  )
}

# Calculator tab server #######################################################
calculator <- function(input, output, session, stringAsFactors) {
  age_in_years <- reactive({
    duration_in_years(input$age_years, input$age_months, input$age_weeks, input$age_days)
  })
  
  output$age_info <- renderText({
    paste("Age:", age_in_years(), "years")
  })
  
  output$height_info <- renderText({
    if (is.numeric(input$height)) {
      lms_stats <- measurement_to_scores(age_in_years(), input$sex, 'ht', input$height)
      stats2string(lms_stats, "Height")
    }
  })
  
  output$weight_info <- renderText({
    if (is.numeric(input$weight)) {
      lms_stats <- measurement_to_scores(age_in_years(), input$sex, 'wt', input$weight)
      stats2string(lms_stats, "Weight")
    }
  })
  
  output$bmi_info <- renderText({
    if (is.numeric(input$bmi)) {
      lms_stats <- measurement_to_scores(age_in_years(), input$sex, 'bmi', input$bmi)
      stats2string(lms_stats, "BMI")
    }
  })
  
  output$sitht_info <- renderText({
    if (is.numeric(input$sitht)) {
      lms_stats <- measurement_to_scores(age_in_years(), input$sex, 'sitht', input$sitht)
      stats2string(lms_stats, "Sitting height")
    }
  })
  
  output$leglen_info <- renderText({
    if (is.numeric(input$legln)) {
      lms_stats <- measurement_to_scores(age_in_years(), input$sex, 'leglen', input$legln)
      stats2string(lms_stats, "Leg length")
    }
  })
}
