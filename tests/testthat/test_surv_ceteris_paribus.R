context("surv_ceteris_paribus")

source("objects_for_tests.R")

test_that("Creating surv_ceteris_paribus", {
  expect_is(cp_cph, "surv_ceteris_paribus_explainer")
  expect_is(cp_cph, "data.frame")
})


test_that("Wrong input",{
  expect_error(ceteris_paribus(surve_cph))
})