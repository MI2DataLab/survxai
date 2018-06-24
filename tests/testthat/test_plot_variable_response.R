context("plot_breakdown")

source("objects_for_tests.R")

test_that("Output", {
  expect_is(plot_curves, "gg")
  expect_is(plot_curves_and_table, "gtable")
})
