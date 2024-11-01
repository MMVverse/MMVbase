library(testthat)

test_that("formula_EC50toEClevel calculates ECx correctly", {
  expect_equal(formula_EC50toEClevel(23, 3, 0.5), 23.0, tolerance = 1e-4)
  expect_equal(formula_EC50toEClevel(23, 10, 0.9), 28.6518, tolerance = 1e-4)
  expect_equal(formula_EC50toEClevel(23, 1, 0.9), 207, tolerance = 1e-4)
})

test_that("formula_EMAXEffect calculates effect correctly", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(formula_EMAXEffect(x, 23, 10), c(0.000000, 0.000000, 0.000000, 0.000000, 0.000000), tolerance = 1e-6)
})

test_that("formula_EMAXmodelParsToMIC calculates MinIC correctly", {
  expect_equal(formula_EMAXmodelParsToMIC(0.1, 0.2, 23, 10), 23)
})

test_that("formula_PRRtoEMAX calculates EMAX correctly", {
  expect_equal(formula_PRRtoEMAX(0.1, 2), 0.19594, tolerance = 1e-4)
})

test_that("formula_EC50toEClevel handles invalid arguments", {
  expect_error(formula_EC50toEClevel("23", 3, 0.5))
  expect_error(formula_EC50toEClevel(23, "3", 0.5))
  expect_error(formula_EC50toEClevel(23, 3, "0.5"))
  expect_error(formula_EC50toEClevel(23, 0, 0.5))
  expect_error(formula_EC50toEClevel(23, 3, -0.5))
  expect_error(formula_EC50toEClevel(23, 3, 1))
})

test_that("formula_EMAXEffect handles invalid arguments", {
  expect_error(formula_EMAXEffect("x", 23, 10))
  expect_error(formula_EMAXEffect(1, "23", 10))
  expect_error(formula_EMAXEffect(1, 23, "10"))
  expect_error(formula_EMAXEffect(-1, 23, 10))
  expect_error(formula_EMAXEffect(1, 0, 10))
  expect_error(formula_EMAXEffect(1, 23, 0))
})

test_that("formula_EMAXmodelParsToMIC handles invalid arguments", {
  expect_error(formula_EMAXmodelParsToMIC(0.1, 0.2, "23", 10))
  expect_error(formula_EMAXmodelParsToMIC(0.1, 0.2, 23, "10"))
  expect_error(formula_EMAXmodelParsToMIC(0.1, 0.2, -23, 10))
  expect_error(formula_EMAXmodelParsToMIC(0.1, 0.2, 23, -10))
})

test_that("formula_PRRtoEMAX handles invalid arguments", {
  expect_error(formula_PRRtoEMAX(0.1, "2"))
  expect_error(formula_PRRtoEMAX("0.1", 2))
  expect_error(formula_PRRtoEMAX(0.1, 2, "48"))
  expect_error(formula_PRRtoEMAX(-0.1, 2))
})