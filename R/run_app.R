#' @include main.R
NULL

#' Run LMSgrowth2 Shiny application
#' 
#'
#' @export
run_app <- function() {
  shiny::shinyApp(ui = .ui,  server = .server)
}
