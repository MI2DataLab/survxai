context("print functions")

source("objects_for_tests.R")

test_that("Output explain", {
  expect_error(print(surve_cph), NA)
})


test_that("Output prediction_breakdown", {
   expect_error(print(broken_prediction), NA)
})

test_that("Output model_performance", {
  expect_error(print(mp_rf), NA)
})

test_that("Output variable_response", {
  expect_error(print(svr_cph), NA)
})

test_that("Output ceteris_paribus", {
  expect_error(print(cp_cph), NA)
})
