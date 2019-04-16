context("test-functions")

test_that("get_height_sds ok", {
  expect_equal(get_height_sds(4, 101.54, 2), 0)
})
