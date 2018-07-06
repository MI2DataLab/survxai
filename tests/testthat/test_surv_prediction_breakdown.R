context("surv_prediction_breakdown")

source("objects_for_tests.R")

test_that("Creating surv_prediction_response", {
  expect_is(broken_prediction, "surv_prediction_breakdown_explainer")
  expect_is(broken_prediction, "data.frame")
})


test_that("Wrong input",{
  expect_error(prediction_breakdown(surve_cph_null_data))
  expect_error(prediction_breakdown(explainer))
})