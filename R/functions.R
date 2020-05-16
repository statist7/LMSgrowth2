#' @importFrom dplyr %>%
NULL

#' returns the value if it is numeric, otherwise a default value
#' @noRd
.get_numeric <- function(value, default) {
  if (!is.numeric(value)) { value <- default }
  value
}

#' Convert a time duration from the given unit to years
#' @noRd
.duration_from_unit_to_years <- function(value, unit) {
  if (unit == "years") {
    value
  } else if (unit == "months") {
    value / 12
  } else if (unit == "weeks") {
    value * 7 / 365.25
  } else if (unit == "days") {
    value / 365.25
  }
}

#' Convert a time duration from years to the given unit
#' @noRd
.duration_from_years_to_unit <- function(value, unit) {
  if (unit == "years") {
    value
  } else if (unit == "months") {
    value * 12
  } else if (unit == "weeks") {
    value * 365.25 / 7
  } else if (unit == "days") {
    value * 365.25
  }
}

#' calculates the duration in days of the arguments
#' @noRd
.duration_in_years <- function(years=c(0), months=c(0), weeks=c(0), days=c(0)) {
  years <- .get_numeric(years, 0)
  months <- .get_numeric(months, 0)
  weeks <- .get_numeric(weeks, 0)
  days <- .get_numeric(days, 0)
  .duration_from_unit_to_years(years, "years") +
    .duration_from_unit_to_years(months, "months") +
    .duration_from_unit_to_years(weeks, "weeks") +
    .duration_from_unit_to_years(days, "days")
}

#' calculates the duration in days of the arguments
#' @noRd
.duration_in_days <- function(years, months, weeks, days) {
  .duration_from_years_to_unit(.duration_in_years(years, months, weeks, days),
                               "days")
}

#' calculates the difference between to dates in years
#' @noRd
.date_diff <- function(date1, date2) {
  .duration_from_unit_to_years(as.numeric(difftime(as.Date(date1), as.Date(date2), units="days")),
                               "days")
}

#' returns the age of the child adjusted by gestational age.
# All ages are in years
#' @noRd
.adjust_age <- function(postnatal_age, gestational_age) {
  if (postnatal_age < 2) {
    postnatal_age + .duration_from_unit_to_years(gestational_age - 40, "weeks")
  } else {
    postnatal_age
  }
}

#' returns the SDS and L M & S values for a given measurement 
#' @noRd
.measurement_to_scores <- function(age_y, sex, measure, value, ref) {
  column_name <- paste('L.', measure, sep='')
  if (!(column_name %in% names(ref))) {
    return(NULL)
  }

  z <- sitar::LMS2z(age_y, value, sex, measure=measure, ref=ref, LMStable = TRUE)
  lmstable <- attr(z, "LMStable")
  list(z=z, L=lmstable$L, M=lmstable$M, S=lmstable$S, value=value, measure=measure)  
  
}

# Function to generate equally spaced SDSs, centred around 0, given their
# number and the step.
#' @noRd
.get_sds_range <- function(ncentiles, step) {
  bound <- (as.integer(ncentiles) - 1) / 2
  (-bound):bound * step
}

#' @noRd
.get_sds <- function(lms_stats) {
  lms_stats$z
}

#' @noRd
.get_centile <- function(lms_stats) {
  sitar::z2cent(lms_stats$z)
}

#' @noRd
.get_perc_predicted <- function(lms_stats) {
  100 * lms_stats$value / lms_stats$M
}

#' @noRd
.get_predicted <- function(lms_stats) {
  lms_stats$M
}

#' @noRd
.get_perc_cv <- function(lms_stats) {
  lms_stats$S * 100
}

#' @noRd
.get_skewness <- function(lms_stats) {
  lms_stats$L
}

#' @noRd
.get_references <- function() {
  list(
    # 'Berkeley Child Guidance Study' = 'berkeley',
    'UK 1990 growth reference' = 'uk90',
    'UK-WHO growth reference including preterm' = 'ukwhopt',
    'UK-WHO growth reference omitting preterm' = 'ukwhoterm',
    'CDC 2000 growth reference' = 'cdc2000',
    'WHO 2006 growth standard' = 'who06',
    'WHO 2006 growth standard & WHO 2007 growth reference' = 'who0607',
    'IOTF international body mass index reference' = 'iotf'
  )
}

#' returns sitar data without loading into current environment
#' @noRd
.get_sitar_data <- function(ref_name) {
  temp_env <- new.env()
  nm <- utils::data(list=ref_name, package="sitar", envir=temp_env)
  temp_env[[nm]]
}

#' all available measure, sitar code and valid ranges for input boxest
#' @noRd
.get_all_measures <- function() {
  list(
    list(description="Height", unit="cm", code="ht", default="", min=20, max=300),
    list(description="Weight", unit="kg", code="wt", default="", min=0.1, max=300),
    list(description="BMI", unit="kg/m\u00B2", code="bmi", default="", min=1, max=300),
    list(description="Head circumference", unit="cm", code="head", default="", min=1, max=300),
    list(description="Sitting height", unit="cm", code="sitht", default="", min=10, max=300),
    list(description="Leg length", unit="cm", code="leglen", default="", min=10, max=300),
    list(description="Waist circumference", unit="cm", code="waist", default="", min=1, max=300),
    list(description="Body fat", unit="% of weight", code="bfat", default="", min=0.1, max=300),
    list(description="Arm circumference", unit="cm", code="arm", default="", min=1, max=300),
    list(description="Subscapular skinfold thickness", unit="mm", code="subscap", default="", min=1, max=300),
    list(description="Tricep skinfold thickness", unit="mm", code="tricep", default="", min=1, max=300)
  )
}

#' returns all measures available in the given sitar dataset (looks at all "L.*" columns)
#' @noRd
.get_measures_from_data <- function(sitar_data) {
  names(sitar_data) %>% stringr::str_subset(., "^L\\.[a-z]*") %>% stringr::str_sub(., 3)
}

#' returns measures lmsgrowth2 knows how to handle for the given sitar dataset
#' @noRd
.get_measures_for_data <- function(sitar_data) {
  .get_all_measures() %>% purrr::keep(function(x) x[['code']] %in% .get_measures_from_data(sitar_data))
}

#' returns the sorted list of ages available in the given sitar dataset
#' @noRd
.get_ages_for_data <- function(sitar_data) {
  sort(sitar_data$years)
}

#' returns the list of sexes available in the given sitar dataset
#' @noRd
.get_sexes_for_data <- function(sitar_data) {
  levels(sitar_data$sex)
}

#' returns the numerical value of centile from the given z-score.  Taken from
#' `sitar::z2cent`.
#' @noRd
.sds_to_centile <- function(z) {
  np <- abs(z) > stats::qnorm(0.99)
  round(stats::pnorm(z) * 100, np)
}

# Shiny UI components for the application

#' To put a title bar across the top of the page, with option to hide sidebar
#' @noRd
.titleBar <- function(id, title, checkboxInputId) {
  if (!is.null(checkboxInputId)) {
    options <- shiny::checkboxGroupInput(checkboxInputId, 
                                           label = NULL, 
                                           choices = list('Sidebar'), 
                                           inline=TRUE, 
                                           selected=c('Sidebar')
      )
  } else {
    options <- NULL
  }
  
  shiny::wellPanel(
    shiny::div(
      tags$head(
        tags$script(
          HTML(stringr::str_interp("Shiny.addCustomMessageHandler ('toggleSidebarPanel', function (message) {  
                                      $(''.concat('#', message, '-sidebarPanel')).toggle(); 
                                      $(''.concat('#', message, '-mainPanel')).toggleClass('col-sm-8 col-sm-12'); 
                                      $(window).trigger('resize')  
                                   });"))
        )
      ),
      shiny::div(shiny::h3(title, style='margin:5px; margin-top:0px; margin-bottom:0px;'), 
                 style='float:left;'),
      shiny::div(options,  style='float:right;'),
      shiny::div(style='clear:both;')
    ), 
    style='padding-bottom:10px;'
  )
}

#' @noRd
.titleBarToggle <- function(label, input, status, session) {
  if ('Sidebar' %in% input & !status$Sidebar | status$Sidebar & (!('Sidebar' %in% input))) {
    session$sendCustomMessage(type='toggleSidebarPanel', message=label)
    status$Sidebar <- !status$Sidebar
  }
}
