library(testthat)

test_that("IsInClosedInterval returns correct results", {
  expect_true(IsInClosedInterval(2, 1, 3))
  expect_true(IsInClosedInterval(2, 1, 2))
  expect_false(IsInClosedInterval(2, 1, 1.5))
  expect_error(IsInClosedInterval(2, 1.5, 1))
})

test_that("IsInOpenInterval returns correct results", {
  expect_true(IsInOpenInterval(2, 1, 3))
  expect_false(IsInOpenInterval(2, 1, 2))
  expect_false(IsInOpenInterval(2, 1, 1.5))
  expect_error(IsInOpenInterval(2, 1.5, 1))
})

test_that("IsInLeftOpenInterval returns correct results", {
  expect_true(IsInLeftOpenInterval(2, 1, 3))
  expect_true(IsInLeftOpenInterval(2, 1, 2))
  expect_false(IsInLeftOpenInterval(2, 1, 1.5))
  expect_error(IsInLeftOpenInterval(2, 1.5, 1))
})

test_that("IsInRightOpenInterval returns correct results", {
  expect_true(IsInRightOpenInterval(2, 1, 3))
  expect_false(IsInRightOpenInterval(2, 1, 2))
  expect_false(IsInRightOpenInterval(2, 1, 1.5))
  expect_error(IsInRightOpenInterval(2, 1.5, 1))
})