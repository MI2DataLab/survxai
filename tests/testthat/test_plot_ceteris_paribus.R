context("plot_ceteris_paribus")

source("objects_for_tests.R")

test_that("Output", {
  expect_is(plot(cp_cph), "ggplot")
  expect_is(plot(cp_cph, selected_variable = "sex"), "ggplot")
})

test_that("Wrong input",{
  expect_error(plot(cp_cph, selected_variable = "se"))
})