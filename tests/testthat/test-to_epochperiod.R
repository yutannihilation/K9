context("to_epochperiod")

test_that("If `from` and `to` are both `NULL`, to_epochperiod returns the period between 1 day ago and now (.split_request = FALSE)", {
  n <- as.integer(lubridate::now())
  expect_equal(to_epochperiod(.split_request = FALSE), c(n - 86400, n))
})

test_that("If only `from` is `NULL`, to_epochperiod returns the period between 1 day before from `to`, and `to`  (.split_request = FALSE)", {
  x <- 1496587909L
  to <- structure(x, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_equal(to_epochperiod(to = to, .split_request = FALSE), c(x - 86400L, x))
})

test_that("If only `to` is `NULL`, to_epochperiod returns the period between `from` and now  (.split_request = FALSE)", {
  x <- 1496500000L
  n <- as.integer(lubridate::now())
  from <- structure(x, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_warning(p <- to_epochperiod(from = from, .split_request = FALSE))
  expect_equal(p, c(x, n))
})

test_that("If both are not `NULL`, to_epochperiod returns the period between `from` and `to`  (.split_request = FALSE)", {
  x <- 1496500000L
  y <- 1496500100L
  from <- structure(x, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  to <- structure(y, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_equal(to_epochperiod(from = from, to = to, .split_request = FALSE), c(x, y))
})

test_that("to_epochperiod automatically split request", {
  x <- 1496500000L
  y <- 1496586500L
  from <- structure(x, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  to <- structure(y, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_equal(to_epochperiod(from = from, to = to), c(1496500000L, 1496586400L, 1496586500L))
})
