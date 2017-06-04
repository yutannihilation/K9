context("to_epochtime")

test_that("to_epochtime with numeric works", {
  expect_equal(to_epochtime(1496587909), 1496587909L)
  expect_equal(to_epochtime(1496587909L), 1496587909L)
})

test_that("to_epochtime with POSIXct and POSIXlt works", {
  x <- structure(1496587909, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_equal(to_epochtime(x), 1496587909L)
  x <- as.POSIXct(x)
  expect_equal(to_epochtime(x), 1496587909L)
})

test_that("to_epochtime with Date works", {
  x <- as.Date("2017-06-04", "%Y-%m-%d", tz = "UTC")
  expect_equal(to_epochtime(x), 1496534400L)
})

test_that("to_epochtime with character works", {
  # timezone is a tough thing...
  testthat::skip_if_not(Sys.timezone() == "Asia/Tokyo")

  x <- "1970-01-01"
  y <- as.integer(lubridate::ymd(x, tz = Sys.timezone()))
  expect_equal(to_epochtime(x), to_epochtime(y))

  x2 <- "1970-01-01 00:00:00"
  expect_equal(to_epochtime(x), to_epochtime(x2))
})

test_that("to_epochtime with NULL throws error", {
  expect_error(to_epochtime(NULL))
})

test_that("to_epochtime with elements more than 1 length throws error", {
  expect_error(to_epochtime(1:2))
})
