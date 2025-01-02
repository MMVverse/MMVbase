library(testthat)
library(MMVbase)

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
test_that("ConvertReportTableToList correctly converts a report table to a list of data.tables", {
  
  table <- NULL
  
  table <- CreateOrAppendToReportTable(table, Style = "subtitle", Text = "PK Parameter Summary (plasma based)")
  table <- CreateOrAppendToReportTable(table, Parameter = "Selected Prediction Method", Value = "IVIVE", Unit = "method")
  table <- CreateOrAppendToReportTable(table, Parameter = "Total Clearance", Human = 20, Unit = "mL/min/kg")
  table <- CreateOrAppendToReportTable(table, Style = "comment", Text = "Hepatic clearance obtained by subtracting renal from total plasma clearance")
  table <- CreateOrAppendToReportTable(table, Parameter = "Hepatic Clearance", Human = 17, Unit = "mL/min/kg")
  
  resultList <- ConvertReportTableToList(table)
  
  expect_equal(length(resultList), 4)
  
  expect_equal(resultList[[1]]$Text, "PK Parameter Summary (plasma based)")
  expect_equal(resultList[[2]]$Parameter, c("Selected Prediction Method", "Total Clearance"))
  expect_equal(resultList[[3]]$Parameter, NA_character_)
})

test_that("get_FromTable returns correct values with FLAG_USE_GREPL", {
  
  table <- NULL
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Selected Prediction Method", Value = "IVIVE", Unit = "method")
  table <- CreateOrAppendToReportTable(table, Parameter = "Total Clearance", Human = 20, Unit = "mL/min/kg")
  table <- CreateOrAppendToReportTable(table, Parameter = "Hepatic Clearance", Human = 17, Unit = "mL/min/kg")
  
  value <- get_FromTable(table, "Selected Prediction Method", FLAG_USE_GREPL = TRUE)
  expect_equal(value, "IVIVE", ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Total Clearance", Column = "Human", FLAG_USE_GREPL = TRUE)
  expect_equal(value, 20, ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Hepatic Clearance", Column = "Human", FLAG_USE_GREPL = TRUE)
  expect_equal(value, 17, ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Renal Clearance", ValueIfNotFound = "Not Found", FLAG_USE_GREPL = TRUE)
  expect_equal(value, "Not Found", ignore_attr = TRUE)
})

test_that("PrintReportTableAsRmd correctly converts a report table to rmd", {
  
  table <- NULL
  
  table <- CreateOrAppendToReportTable(table, Style = "subtitle", Text = "PK Parameter Summary (plasma based)")
  table <- CreateOrAppendToReportTable(table, Parameter = "Selected Prediction Method", Value = "IVIVE", Unit = "method")
  table <- CreateOrAppendToReportTable(table, Parameter = "Total Clearance", Human = 20, Unit = "mL/min/kg")
  table <- CreateOrAppendToReportTable(table, Style = "comment", Text = "Hepatic clearance obtained by subtracting renal from total plasma clearance")
  table <- CreateOrAppendToReportTable(table, Parameter = "Hepatic Clearance", Human = 17, Unit = "mL/min/kg")
  
  expect_output(PrintReportTableAsRmd(table), "PK Parameter Summary \\(plasma based\\)")
  expect_output(PrintReportTableAsRmd(table), "Selected Prediction Method")
  expect_output(PrintReportTableAsRmd(table), "Total Clearance")
  expect_output(PrintReportTableAsRmd(table), "Hepatic clearance obtained by subtracting renal from total plasma clearance")
  expect_output(PrintReportTableAsRmd(table), "Hepatic Clearance")
})

test_that("get_FromTable returns correct values with different columns", {
  
  table <- NULL
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Selected Prediction Method", Value = "IVIVE", Unit = "method")
  table <- CreateOrAppendToReportTable(table, Parameter = "Total Clearance", Human = 20, Unit = "mL/min/kg")
  table <- CreateOrAppendToReportTable(table, Parameter = "Hepatic Clearance", Human = 17, Unit = "mL/min/kg")
  
  value <- get_FromTable(table, "Selected Prediction Method", Column = "Value")
  expect_equal(value, "IVIVE", ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Total Clearance", Column = "Human")
  expect_equal(value, 20, ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Hepatic Clearance", Column = "Human")
  expect_equal(value, 17, ignore_attr = TRUE)
  
  value <- get_FromTable(table, "Renal Clearance", Column = "Human", ValueIfNotFound = "Not Found")
  expect_equal(value, "Not Found", ignore_attr = TRUE)
})

test_that("CreateOrAppendToReportTable handles NULL values correctly", {
  
  table <- NULL
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Selected Prediction Method", Value = NULL, Unit = "method")
  expect_equal(nrow(table), 1)
  expect_true(is.null(table$Value[1]))
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Total Clearance", Human = NULL, Unit = "mL/min/kg")
  expect_equal(nrow(table), 2)
  expect_true(is.null(table$Human[2]))
  
  table <- CreateOrAppendToReportTable(table, Parameter = "Hepatic Clearance", Human = 17, Unit = NULL)
  expect_equal(nrow(table), 3)
  expect_true(is.na(table$Unit[3]))
})

