library(testthat)
library(MMVbase)

test_that("formula_convertConc throws an error when 'from' is not a valid option", {
  Cin <- 10
  Rbp_from <- 1
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- 0.2
  
  expect_error(formula_convertConc(Cin, from = "invalid", to = "unbound", Rbp_from, PPB_from, Rbp_to, PPB_to), 
               "Invalid value for 'from'. Must be one of blood, plasma, unbound, medium")
})

test_that("formula_convertConc throws an error when 'to' is not a valid option", {
  Cin <- 10
  Rbp_from <- 1
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- 0.2
  
  expect_error(formula_convertConc(Cin, from = "blood", to = "invalid", Rbp_from, PPB_from, Rbp_to, PPB_to), 
               "Invalid value for 'to'. Must be one of blood, plasma, unbound, medium")
})

test_that("formula_convertConc throws an error when 'PPB_from' is not between 0 and 1", {
  Cin <- 10
  Rbp_from <- 1
  PPB_from <- 1.5
  Rbp_to <- 0.8
  PPB_to <- 0.2
  
  expect_error(formula_convertConc(Cin, from = "blood", to = "unbound", Rbp_from, PPB_from, Rbp_to, PPB_to), 
               "Invalid value for 'PPB_from'. Must be between 0 and 1")
})

test_that("formula_convertConc throws an error when 'PPB_to' is not between 0 and 1", {
  Cin <- 10
  Rbp_from <- 1
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- -0.5
  
  expect_error(formula_convertConc(Cin, from = "blood", to = "unbound", Rbp_from, PPB_from, Rbp_to, PPB_to), 
               "Invalid value for 'PPB_to'. Must be between 0 and 1")
})

test_that("formula_convertConc throws an error when 'PPB_to' is 1 and 'to' is not 'unbound'", {
  Cin <- 10
  Rbp_from <- 1
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- 1
  
  expect_error(formula_convertConc(Cin, from = "blood", to = "plasma", Rbp_from, PPB_from, Rbp_to, PPB_to), 
               "Invalid value for 'PPB_to'. When to is not 'unbound' PPB_to cannot be 1")
})

test_that("formula_convertConc throws an error when 'Rbp_from' is 0 and 'from' is 'blood'", {
  Cin <- 10
  Rbp_from <- 0
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- 0.2
  
  expect_error(formula_convertConc(Cin, from = "blood", to = "unbound", Rbp_from, PPB_from, Rbp_to, PPB_to), 
               "Invalid value for 'Rbp_from'. When from is 'blood' Rbp_from cannot be 0")
})

test_that("formula_convertConc converts concentration from blood to unbound then to plasma correctly", {
  Cin <- 10
  Rbp_from <- 2
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- 0.2
  
  expected_unbound <- Cin * (1 - PPB_from) / Rbp_from
  result_unbound <- formula_convertConc(Cin, from = "blood", to = "unbound", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_unbound, expected_unbound)
  
  expected_plasma <- result_unbound / (1 - PPB_to)
  result_plasma <- formula_convertConc(result_unbound, from = "unbound", to = "plasma", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_plasma, expected_plasma)
    
  expected_plasma <- Cin * (1 - PPB_from) / (1 - PPB_to) / Rbp_from
  result_plasma <- formula_convertConc(Cin, from = "blood", to = "plasma", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_plasma, expected_plasma)
})

test_that("formula_convertConc converts concentration from blood to unbound then to medium correctly", {
  Cin <- 10
  Rbp_from <- 2
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- 0.2

  expected_unbound <- Cin * (1 - PPB_from) / Rbp_from
  result_unbound <- formula_convertConc(Cin, from = "blood", to = "unbound", Rbp_from, PPB_from, Rbp_to, PPB_to)

  expect_equal(result_unbound, expected_unbound)

  expected_medium <- result_unbound / (1 - PPB_to)
  result_medium <- formula_convertConc(result_unbound, from = "unbound", to = "medium", Rbp_from, PPB_from, Rbp_to, PPB_to)

  expect_equal(result_medium, expected_medium)

  expected_medium <- Cin * (1 - PPB_from) / (1 - PPB_to) / Rbp_from
  result_medium <- formula_convertConc(Cin, from = "blood", to = "medium", Rbp_from, PPB_from, Rbp_to, PPB_to)

  expect_equal(result_medium, expected_medium)
})

test_that("formula_convertConc converts concentration from plasma to unbound then to blood correctly", {
  Cin <- 10
  Rbp_from <- 2
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- 0.2
  
  expected_unbound <- Cin * (1 - PPB_from)
  result_unbound <- formula_convertConc(Cin, from = "plasma", to = "unbound", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_unbound, expected_unbound)
  
  expected_blood <- result_unbound / (1 - PPB_to) * Rbp_to
  result_blood <- formula_convertConc(result_unbound, from = "unbound", to = "blood", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_blood, expected_blood)
    
  expected_blood <- Cin * (1 - PPB_from) / (1 - PPB_to) * Rbp_to
  result_blood <- formula_convertConc(Cin, from = "plasma", to = "blood", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_blood, expected_blood)
})

test_that("formula_convertConc converts concentration from medium to unbound then to blood correctly", {
  Cin <- 10
  Rbp_from <- 2
  PPB_from <- 0.1
  Rbp_to <- 0.8
  PPB_to <- 0.2
  
  expected_unbound <- Cin * (1 - PPB_from)
  result_unbound <- formula_convertConc(Cin, from = "medium", to = "unbound", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_unbound, expected_unbound)
  
  expected_blood <- result_unbound / (1 - PPB_to) * Rbp_to
  result_blood <- formula_convertConc(result_unbound, from = "unbound", to = "blood", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_blood, expected_blood)
    
  expected_blood <- Cin * (1 - PPB_from) / (1 - PPB_to) * Rbp_to
  result_blood <- formula_convertConc(Cin, from = "medium", to = "blood", Rbp_from, PPB_from, Rbp_to, PPB_to)
  
  expect_equal(result_blood, expected_blood)
})
