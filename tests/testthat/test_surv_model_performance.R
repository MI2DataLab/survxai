context("surv_model_performance")

source("objects_for_tests.R")

test_that("Creating surv_model_performance", {
  expect_is(mp_cph, "surv_model_performance_explainer")
  expect_is(mp_cph, "BS")
})

test_that("Wrong input",{
  expect_error(model_performance(cph_model))
  expect_error(model_performance(surve_cph_null_data))
})