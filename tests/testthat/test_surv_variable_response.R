context("surv_variable_response")

source("objects_for_tests.R")

test_that("Creating surv_variable_response", {
  expect_is(svr_cph, "surv_variable_response_explainer")
  expect_is(svr_cph, "pdp")
  expect_is(svr_cph, "data.frame")
})


test_that("Wrong input",{
  expect_error(variable_response(surve_cph_null_data))
  expect_error(variable_response(explainer))
})
