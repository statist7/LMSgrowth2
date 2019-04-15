# simple test using a sitar function
library(sitar)

get_height_sds <- function(age_y, height_cm, sex) {
  sitar::LMS2z(age_y, height_cm, sex, measure='ht', ref='uk90')
}
