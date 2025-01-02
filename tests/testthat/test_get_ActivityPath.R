library(testthat)
library(MMVbase)

test_that("get_ActivityPath returns correct path when ActivityPath is provided", {
  expect_equal(get_ActivityPath("custom/path"), "custom/path")
})

test_that("get_ActivityPath returns correct path when tags.RData exists", {
  # Create a mock tags.RData file
  tags <- list(itemPath = "/sites/department/ModellingTeam/ProjectX", 
               itemName = "ActivityY")
  save(tags, file = "../tags.RData")
  
  expect_equal(get_ActivityPath(), "ProjectX/ActivityY")
  
  # Clean up
  unlink("../tags.RData")
})

test_that("get_ActivityPath returns correct path when in Projects folder", {
  # Mock getwd() to return a path in Projects
  mock_getwd <- function() "/Projects/CompX/Work/A01_PK/Scripts"
  with_mocked_bindings(getwd = mock_getwd, .package = "base",
                       expect_equal(get_ActivityPath(), "CompX/Work/A01_PK"))
  
})

test_that("get_ActivityPath returns correct path when in Projects_Discovery folder", {
  # Mock getwd() to return a path in Projects
  mock_getwd <- function() "/Projects_Discovery/Serie1/Work/CompX/A01_PK/Scripts"
  with_mocked_bindings(getwd = mock_getwd, .package = "base",
                       expect_equal(get_ActivityPath(), "Serie1/Work/CompX/A01_PK"))
})

test_that("get_ActivityPath returns NULL and warns when not in project folder", {
  # Mock getwd() to return a path not in project folder
  mock_getwd <- function() "/some/other/path"
  with_mocked_bindings(getwd = mock_getwd, .package = "base",
                       expect_warning(result <- get_ActivityPath(),
                                      "The current activity is not in the project folder"))
  expect_equal(result, "NULL")
})
