context("surv_variable_response")

source("objects_for_tests.R")

test_that("Creating surv_variable_response", {
  expect_is(svr_cph, "surv_variable_response_explainer")
  expect_is(svr_cph, "pdp")
  expect_is(svr_cph, "data.frame")
})

