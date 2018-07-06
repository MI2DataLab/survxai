context("plot_variable_response")

source("objects_for_tests.R")

test_that("Output", {
  expect_is(plot(broken_prediction), "ggplot")
  expect_is(plot(broken_prediction, broken_prediction2), "ggplot")
})