context("test-functions")

test_that("get_height_sds ok", {
  lms_stats <- .measurement_to_scores(4, 2, 'ht', 101.54, 'uk90')
  expect_equal(lms_stats$z[1], 0)
})
