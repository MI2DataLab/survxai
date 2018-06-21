context("surv_breakdown_results")

source("objects_for_tests.R")

test_that("Creating surv_breakdown", {
  expect_is(broken_list, "surv_breakdown")
})

