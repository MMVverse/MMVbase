library(testthat)

test_that(paste0(
  "CreateOrAppendToReportTable appends rows correctly", 
  " and get_FromTable can access the correct values"), {
  
  table <- NULL
  
  table <- CreateOrAppendToReportTable(table, Style = "subtitle", Text = "PK Parameter Summary (plasma based)")
  expect_equal(nrow(table), 1)
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Selected Prediction Method", Value = "IVIVE", Unit = "method")
  expect_equal(nrow(table), 2)
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Total Clearance", Human = 20, Unit = "mL/min/kg")
  expect_equal(nrow(table), 3)
  
  table <- CreateOrAppendToReportTable(table, Style = "comment", Text = "Hepatic clearance obtained by subtracting renal from total plasma clearance")
  expect_equal(nrow(table), 4)
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Hepatic Clearance", Human = 17, Unit = "mL/min/kg")
  expect_equal(nrow(table), 5)

  value <- get_FromTable(table, "Selected Prediction Method")
  expect_equal(value, "IVIVE",  ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Total Clearance",  Column = "Human")
  expect_equal(value, 20, ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Hepatic Clearance", Column = "Human")
  expect_equal(value, 17, ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Renal Clearance", ValueIfNotFound = "Not Found")
  expect_equal(value, "Not Found", ignore_attr = TRUE)
})
test_that("CreateOrAppendToReportTable appends rows correctly and get_FromTable can access the correct values", {
  
  table <- NULL
  
  table <- CreateOrAppendToReportTable(table, Style = "subtitle", Text = "PK Parameter Summary (plasma based)")
  expect_equal(nrow(table), 1)
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Selected Prediction Method", Value = "IVIVE", Unit = "method")
  expect_equal(nrow(table), 2)
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Total Clearance", Human = 20, Unit = "mL/min/kg")
  expect_equal(nrow(table), 3)
  
  table <- CreateOrAppendToReportTable(table, Style = "comment", Text = "Hepatic clearance obtained by subtracting renal from total plasma clearance")
  expect_equal(nrow(table), 4)
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Hepatic Clearance", Human = 17, Unit = "mL/min/kg")
  expect_equal(nrow(table), 5)

  value <- get_FromTable(table, "Selected Prediction Method")
  expect_equal(value, "IVIVE",  ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Total Clearance",  Column = "Human")
  expect_equal(value, 20, ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Hepatic Clearance", Column = "Human")
  expect_equal(value, 17, ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Renal Clearance", ValueIfNotFound = "Not Found")
  expect_equal(value, "Not Found", ignore_attr = TRUE)
})