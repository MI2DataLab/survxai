context("surv_explainers")

source("objects_for_tests.R")

test_that("plotACF", {
  expect_is(surve_cph, "surv_explainer")
})
