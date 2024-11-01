library(testthat)

test_that("GeometricMean calculates the geometric mean correctly", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(MMVbase::GeometricMean(x), 2.605171,  tolerance = 1e-6)
  
  x <- c(1, 2, 3, 4, 5, -1)
  expect_warning(MMVbase::GeometricMean(x))
  
  x <- c(1, 2, 3, 4, 5, NA)
  expect_equal(MMVbase::GeometricMean(x), NA_real_)
  
  x <- c(1, 2, 3, 4, 5, NA)
  expect_equal(MMVbase::GeometricMean(x, na.rm = TRUE), 2.605171, tolerance = 1e-6)
  
  x <- c(1, 2, 3, 4, 5, NA, 0)
  expect_warning(MMVbase::GeometricMean(x))
})

test_that("GeometricSD calculates the geometric standard deviation correctly", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(GeometricSD(x), 1.887984, tolerance = 1e-6)
  
  y <- c(10, 100, 1000, 10000)
  expect_equal(GeometricSD(y), 19.54314, tolerance = 1e-6)
  
  z <- c(0.5, 1, 2, 4, 8)
  expect_equal(GeometricSD(z), 2.992059, tolerance = 1e-6)
  
  w <- c(1, 2, NA, 4, 5)
  expect_equal(GeometricSD(w, na.rm = TRUE), 2.071241, tolerance = 1e-6)
  
  v <- c(1, 2, 3, 4, 5)
  expect_equal(GeometricSD(v, sqrt.unbiased = FALSE), 1.76547, tolerance = 1e-6)
  
  u <- c(1, 2, 3, 4, 5, NA)
  expect_equal(GeometricSD(u, na.rm = TRUE, sqrt.unbiased = FALSE), 1.76547, tolerance = 1e-6)

  t <- c(1, 2, 3, 4, 5, NA, 0)
  expect_warning(GeometricSD(t, na.rm = TRUE, sqrt.unbiased = FALSE))
})