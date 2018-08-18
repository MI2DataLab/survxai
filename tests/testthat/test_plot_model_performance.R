context("plot_model_performance")

source("objects_for_tests.R")

test_that("Output", {
  expect_is(plot(mp_cph), "gg")
  expect_is(plot(mp_cph, model_performance(surve_cph2, data = pbcTest)), "gg")
  expect_is(plot(mp_cph, mp_cph_artificial), "gg")
})