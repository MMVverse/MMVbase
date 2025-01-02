library(testthat)
library(MMVbase)

test_that("formula_CLintuToCLhPl calculates CLh correctly", {
  # Define input parameters
  CLintu <- 10  # Unbound intrinsic clearance
  Qh <- 20      # Blood flow rate through the liver
  Fu <- 0.5    # Fraction of drug unbound in plasma
  BP <- 2.23

  # Check if the result matches the expected output
  expect_equal(formula_CLintuToCLhPl(CLintu, Qh, Fu, BP), 4.495968, tolerance = 1e-4)
})

test_that("formula_CLintuToCLhPl handles edge cases", {
  # Edge case: CLintu is zero
  CLintu <- 0
  Qh <- 20
  Fu <- 0.5
  BP <- 1
  expected_CLh <- 0
  result <- formula_CLintuToCLhPl(CLintu, Qh, Fu, BP)
  expect_equal(result, expected_CLh)
  
  # Edge case: Fu is zero
  CLintu <- 10
  Qh <- 20
  Fu <- 0
  BP <- 1
  expected_CLh <- 0
  result <- formula_CLintuToCLhPl(CLintu, Qh, Fu, BP)
  expect_equal(result, expected_CLh)
  
  # Edge case: Qh is zero
  CLintu <- 10
  Qh <- 0
  Fu <- 0.5
  BP <- 1
  expected_CLh <- 0
  result <- formula_CLintuToCLhPl(CLintu, Qh, Fu, BP)
  expect_equal(result, expected_CLh)
})

test_that("formula_CLhPlToCLintu calculates CLintu correctly", {
  # Define input parameters
  CLh <- 10    # Hepatic blood clearance
  Qh <- 20     # Blood flow rate through the liver
  Fu <- 0.5   # Fraction of drug unbound in plasma
  BP <- 1
  
  # Check if the result matches the expected output
  expect_equal(formula_CLhPlToCLintu(CLh, Qh, Fu, BP), 40)
})

test_that("formula_CLhPlToCLintu handles edge cases", {
  # Edge case: CLh is zero
  CLh <- 0
  Qh <- 20
  Fu <- 0.5
  BP <- 1
  expect_equal(formula_CLhPlToCLintu(CLh, Qh, Fu, BP), 0)
  
  # Edge case: Fu is zero
  CLh <- 10
  Qh <- 20
  Fu <- 0
  BP <- 1.2
  expect_equal(formula_CLhPlToCLintu(CLh, Qh, Fu, BP), Inf)
  
  # Edge case: Qh is zero
  CLh <- 10
  Qh <- 0
  Fu <- 0.5
  BP <- 1
  expect_equal(formula_CLhPlToCLintu(CLh, Qh, Fu, BP), 0)
  
  # Edge case: Qh is equal ot CLh
  CLh <- 10
  Qh <- 10
  Fu <- 0.5
  BP <- 1
  expect_equal(formula_CLhPlToCLintu(CLh, Qh, Fu, BP), Inf)
})


test_that("formula_EC50toEClevel calculates ECx correctly", {
  expect_equal(formula_EC50toEClevel(23, 3, 0.5), 23.0, tolerance = 1e-4)
  expect_equal(formula_EC50toEClevel(23, 10, 0.9), 28.6518, tolerance = 1e-4)
  expect_equal(formula_EC50toEClevel(23, 1, 0.9), 207, tolerance = 1e-4)
})

test_that("formula_EMAXEffect calculates effect correctly", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(formula_EMAXEffect(x, 2, 3), 
               c(0.1111111, 0.5000000, 0.7714286, 0.8888889, 0.9398496), tolerance = 1e-6)
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
test_that("formula_CLintuToCLh_Parallel calculates CLh correctly", {
  expect_equal(formula_CLintuToCLh_Parallel(10, 1000, 0.5), 4.987521, tolerance = 1e-4)
  expect_equal(formula_CLintuToCLh_Parallel(20, 1000, 0.5), 9.950166, tolerance = 1e-4)
  expect_equal(formula_CLintuToCLh_Parallel(30, 1000, 0.5), 14.88806, tolerance = 1e-4)
})

test_that("formula_CLhToCLintu_Parallel calculates CLintu correctly", {
  expect_equal(formula_CLhToCLintu_Parallel(4.987521, 1000, 0.5), 10, tolerance = 1e-4)
  expect_equal(formula_CLhToCLintu_Parallel(9.950166, 1000, 0.5), 20, tolerance = 1e-4)
  expect_equal(formula_CLhToCLintu_Parallel(14.88806, 1000, 0.5), 30, tolerance = 1e-4)
})

test_that("formula_CLintuToCLh_WellStirred calculates CLh correctly", {
  expect_equal(formula_CLintuToCLh_WellStirred(10, 1000, 0.5), 4.975124, tolerance = 1e-4)
  expect_equal(formula_CLintuToCLh_WellStirred(20, 1000, 0.5), 9.90099, tolerance = 1e-4)
  expect_equal(formula_CLintuToCLh_WellStirred(30, 1000, 0.5), 14.77833, tolerance = 1e-4)
})

test_that("formula_CLhToCLintu_WellStirred calculates CLintu correctly", {
  expect_equal(formula_CLhToCLintu_WellStirred(4.975124, 1000, 0.5), 10, tolerance = 1e-4)
  expect_equal(formula_CLhToCLintu_WellStirred(9.90099, 1000, 0.5), 20, tolerance = 1e-4)
  expect_equal(formula_CLhToCLintu_WellStirred(14.77833, 1000, 0.5), 30, tolerance = 1e-4)
})

test_that("formula_CLintuToCLh handles invalid arguments", {
  expect_error(formula_CLintuToCLh("10", 1000, 0.5, "Well-Stirred"))
  expect_error(formula_CLintuToCLh(10, "1000", 0.5, "Well-Stirred"))
  expect_error(formula_CLintuToCLh(10, 1000, "0.5", "Well-Stirred"))
  expect_error(formula_CLintuToCLh(10, 1000, 0.5, "InvalidModel"))
})

test_that("formula_CLhToCLintu handles invalid arguments", {
  expect_error(formula_CLhToCLintu("10", 1000, 0.5, "Well-Stirred"))
  expect_error(formula_CLhToCLintu(10, "1000", 0.5, "Well-Stirred"))
  expect_error(formula_CLhToCLintu(10, 1000, "0.5", "Well-Stirred"))
  expect_error(formula_CLhToCLintu(10, 1000, 0.5, "InvalidModel"))
})

test_that("formula_HepExtraction calculates extraction ratio correctly", {
  expect_equal(formula_HepExtraction(23, 1000), 0.023, tolerance = 1e-4)
  expect_equal(formula_HepExtraction(50, 1000), 0.05, tolerance = 1e-4)
  expect_equal(formula_HepExtraction(100, 1000), 0.1, tolerance = 1e-4)
})

test_that("formula_HepExtraction handles invalid arguments", {
  expect_error(formula_HepExtraction("23", 1000))
  expect_error(formula_HepExtraction(23, "1000"))
})
