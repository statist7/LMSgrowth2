#-------------------------------------------------------------------------------
# The ui and server Shiny components to interact with LMS densities
#-------------------------------------------------------------------------------

.densityUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    .titleBar('density', 'LMS density', ns('titleBar')),
    sidebarLayout(
      sidebarPanel(
        h3("LMS density"),
        sliderInput(ns('L'), 'L', value = 1, min = -4, max = 6, step = 0.1, ticks = TRUE),
        shinyWidgets::sliderTextInput(ns("M"),"M",
                                      choices=c(outer(c(1, 2, 5), 10^(-3:2)), 1e3),
                                      selected=1, grid = TRUE),
        sliderInput(ns('S'), 'S', value = 0.2, min = 0, max = 1, step = 0.01, ticks = TRUE),
        textInput(ns('centiles'), 'distribution centiles', 
                  paste(gsub('[a-z]', '', sitar::z2cent(-4:4*2/3)), collapse=' ')),
        id='density-sidebarPanel'
      ),
      mainPanel(
        plotOutput(ns('plotout')),
        id='density-mainPanel'
      )
    )
  )
}

.density <- function(input, output, session, globals) {
  ns <- session$ns
  output$plotout <- renderPlot({
    validate(need(input$M > 0, "M must be > 0"))
    validate(need(input$S > 0, "S must be > 0"))
    zcent <- as.numeric(unlist(strsplit(input$centiles,"[,; \t]+")))
    if (length(zcent) == 0)
      zcent <- NULL
    else
      zcent <- stats::qnorm(zcent/100)
    sitar::pdLMS(L = input$L, M = input$M, S = input$S, zcent = zcent, pch = 25, las = 1)
  })
  
  # handle the sidebar show/hide
  uiStatus <- reactiveValues(Sidebar=TRUE)
  observeEvent(input$titleBar,
               .titleBarToggle('density', input$titleBar, uiStatus, session),
               ignoreNULL=FALSE)
}
