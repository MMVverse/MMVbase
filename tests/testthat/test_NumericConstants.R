library(testthat)

test_that("get_NumericConstantsAsList returns a named list", {
  constants <- get_NumericConstantsAsList()
  expect_type(constants, "list")
  expect_true(length(names(constants)) > 0)
})

test_that("get_NumericConstants returns a data.frame", {
  constants <- get_NumericConstants()
  expect_s3_class(constants, "data.frame")
  expect_true(ncol(constants) == 5)
  expect_true(nrow(constants) > 0)
})

test_that("get_NumericConstantsAsList and get_NumericConstants have matching values", {
  listConstants <- get_NumericConstantsAsList()
  dfConstants <- get_NumericConstants()
  
  expect_equal(length(listConstants), nrow(dfConstants))
  
  for (i in 1:length(listConstants)) {
    name <- names(listConstants)[i]
    value <- listConstants[[i]]
    expect_equal(value, dfConstants$Value[dfConstants$Name_unit == name])
  }
})

test_that("parasite growth rate constants are defined", {
  constants <- get_NumericConstantsAsList()
  expect_equal(constants$PLGrowthRateInVitro_1PerHour, log(10) / 48, tolerance = 1e-7)
  expect_equal(constants$PLGrowthRatePatient_1PerHour, 0.07)
  expect_equal(constants$PLGrowthRateSCID_1PerHour, 0.03)
})
