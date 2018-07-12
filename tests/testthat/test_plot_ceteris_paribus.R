context("plot_ceteris_paribus")

source("objects_for_tests.R")

test_that("Output", {
  expect_is(plot(cp_cph), "ggplot")
})