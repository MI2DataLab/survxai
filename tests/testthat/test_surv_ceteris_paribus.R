context("surv_ceteris_paribus")

source("objects_for_tests.R")

test_that("Creating surv_ceteris_paribus", {
  expect_is(cp_cph, "surv_ceteris_paribus_explainer")
  expect_is(cp_cph, "data.frame")
  expect_is(ceteris_paribus(surve_cph, pbc[1,-c(1,2)], selected_variables = "sex"), "data.frame")
})


test_that("Wrong input",{
  expect_error(ceteris_paribus(surve_cph))
  expect_error(ceteris_paribus(pbc[1,-c(1,2)]))
  expect_error(ceteris_paribus(surve_cph_null_data, pbc[1,-c(1,2)]))
})