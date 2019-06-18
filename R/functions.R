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

#' returns the SDS and L M & S values for a given measurement 
.measurement_to_scores <- function(age_y, sex, measure, value) {
  z <- sitar::LMS2z(age_y, value, sex, measure=measure, ref=sitar::uk90, LMStable = TRUE)
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
