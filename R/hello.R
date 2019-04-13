# simple test using a sitar function

hello <- function() {
    data(heights)
    data(uk90)
    with(heights, LMS2z(age, height, sex = 2, measure ='ht', ref ='uk90'))
}
