#' returns the value if it is numeric, otherwise a default value
.get_numeric <- function(value, default) {
  if (!is.numeric(value)) { value <- default }
  value
}

#' calculates the duration in days of the arguments
.duration_in_years <- function(years=c(0), months=c(0), weeks=c(0), days=c(0)) {
  years <- .get_numeric(years, 0)
  months <- .get_numeric(months, 0)
  weeks <- .get_numeric(weeks, 0)
  days <- .get_numeric(days, 0)
  (years) + (months / 12) + (weeks * 7 / 365.25) + (days / 365.25)
}

#' calculates the duration in days of the arguments
.duration_in_days <- function(years, months, weeks, days) {
  .duration_in_years(years, months, weeks, days) * 365.25
}

#' calculates the difference between to dates in years
.date_diff <- function(date1, date2) {
  as.numeric(difftime(as.Date(date1), as.Date(date2), unit="days")) / 365.25  
}

#' returns the SDS and L M & S values for a given measurement 
.measurement_to_scores <- function(age_y, sex, measure, value, ref) {
  selected_ref <- getExportedValue('sitar', ref)
  column_name <- paste('L.', measure, sep='')
  if (!(column_name %in% names(selected_ref))) {
    return(NULL)
  }

  z <- sitar::LMS2z(age_y, value, sex, measure=measure, ref=selected_ref, LMStable = TRUE)
  lmstable <- attr(z, "LMStable")
  list(z=z, L=lmstable$L, M=lmstable$M, S=lmstable$S, value=value, measure=measure)  
  
}

.get_sds <- function(lms_stats) {
  lms_stats$z
}

.get_centile <- function(lms_stats) {
  sitar::z2cent(lms_stats$z)
}

.get_perc_predicted <- function(lms_stats) {
  100 * lms_stats$value / lms_stats$M
}

.get_predicted <- function(lms_stats) {
  lms_stats$M
}

.get_perc_cv <- function(lms_stats) {
  lms_stats$S * 100
}

.get_skewness <- function(lms_stats) {
  lms_stats$L
}

.get_references <- function() {
  list(
    # 'Berkeley Child Guidance Study' = 'berkeley',
    'UK 1990 growth reference' = 'uk90',
    'UK-WHO growth reference (w/ preterm)' = 'ukwhopt',
    'WHO 2006 growth standard' = 'who06'
  )
}

#' returns sitar data without loading into current environment
.get_sitar_data <- function(ref_name) {
  temp_env <- new.env()
  nm <- data(list=ref_name, package="sitar", envir=temp_env)
  temp_env[[nm]]
}

#' all available measure, sitar code and valid ranges for input boxes
.get_all_measures <- function() {
  list(
    list(description='Height', unit='cm', code='ht', default='', min=20, max=300),
    list(description='Weight', unit='kg', code='wt', default='', min=0.1, max=300),
    list(description='BMI', unit='kg/m^2', code='bmi', default='', min=1, max=300),
    list(description='Sitting height', unit='cm', code='sitht', default='', min=10, max=300),
    list(description='Leg length', unit='cm', code='leglen', default='', min=10, max=300),
    list(description='Body fat', unit='xx', code='bfat', default='', min=0.1, max=300)
  )
}

#' returns all measures available in the given sitar dataset (looks at all "L.*" columns)
.get_measures_from_ref <- function(reference) {
  names(.get_sitar_data(reference)) %>% str_subset(., "^L\\.[a-z]*") %>% str_sub(., 3)
}

#' returns measures lmsgrowth2 knows how to handle for the given sitar dataset
.get_measures_for_ref <- function(reference) {
  .get_all_measures() %>% keep(function(x) x[['code']] %in% .get_measures_from_ref(reference))
}

#' constructs a string to display sds & other information about a measurement
.stats2string <- function(lms_stats, title="") {
  # quick and dirty output for now
  labels <- matrix(
    c('SDS', .get_sds(lms_stats), 
      'Centile', .get_centile(lms_stats),
      '% Predicted', .get_perc_predicted(lms_stats),
      'Predicted', .get_predicted(lms_stats),
      '% CV', .get_perc_cv(lms_stats),
      'Skewness', .get_skewness(lms_stats)
    ), byrow = T, ncol = 2
  )
  output <- apply(labels, 1, function(x) paste(x[1], x[2]))
  output <- paste(c(title, output))
  output <- paste(output, collapse = "    \n")
  output
}
