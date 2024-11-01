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