context("plot_variable_response")

source("objects_for_tests.R")

test_that("Output", {
  expect_is(plot(svr_cph_group), "gg")
  expect_is(plot(svr_cph), "gg")
  expect_is(plot_var_resp_levels, "ggplot")
  expect_is(plot_var_resp_default, "ggplot")
})
