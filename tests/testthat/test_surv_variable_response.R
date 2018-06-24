context("surv_variable_response")

source("objects_for_tests.R")

test_that("Creating surv_variable_response", {
  expect_is(svr_cph, "surv_variable_response")
})

