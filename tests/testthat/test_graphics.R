library(testthat)
library(ggplot2)

test_that("MMVggplot returns a ggplot object with default caption, when not run from an activity", {
  # There should be a warning for the activity path
  expect_warning(result <- MMVggplot(mtcars, aes(x = wt, y = mpg)) + geom_point())
  # Check if the result is a ggplot object
  expect_s3_class(result, "ggplot")
  expect_identical(result$labels$caption, "Activity: NULL")
  expect_true(result$theme$plot.caption$size == 10)
})

test_that("MMVggplot returns a ggplot object with MMVvpc style", {
  # Create a sample ggplot object
  expect_warning(result <- MMVggplot(mtcars, aes(x = wt, y = mpg), style = "MMVvpc") + geom_point())
  # Check if the result is a ggplot object
  expect_s3_class(result, "ggplot")
  # Check if the theme is adjusted to MMVvpc style
  expect_true(result$theme$plot.caption$size == 8)
})

test_that("MMVggplot handles invalid style gracefully", {
  # Apply MMVggplot function with an invalid style
  expect_warning(
    result <- MMVggplot(mtcars, aes(x = wt, y = mpg), style = "invalid_style") + geom_point())
  # Check if the result is a ggplot object
  expect_s3_class(result, "ggplot")
  # Check if the theme is adjusted to MMVvpc style
  expect_null(result$theme$plot.caption$size)
})
