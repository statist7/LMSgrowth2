context('text-shinyapp')

library(testthat)

test_that("all shiny tests pass", {
  shinytest::expect_pass(shinytest::testApp('../..', compareImages = FALSE))
})
