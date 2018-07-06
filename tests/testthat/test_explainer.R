context("surv_explainer")

source("objects_for_tests.R")

test_that("Creating surv_explainer", {
  expect_is(surve_cph, "surv_explainer")
})
