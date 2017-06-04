context("to_epochperiod")

test_that("If `from` and `to` are both `NULL`, to_epochperiod returns the period between 1 day ago and now", {
  n <- as.integer(lubridate::now())
  expect_equal(to_epockperiod(), c(n - 86400, n))
})


test_that("If only `from` is `NULL`, to_epochperiod returns the period between 1 day before from `to`, and `to`.", {
  x <- 1496587909L
  to <- structure(x, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_equal(to_epockperiod(to = to), c(x - 86400L, x))
})

test_that("If only `to` is `NULL`, to_epochperiod returns the period between `from` and now.", {
  x <- 1496500000L
  n <- as.integer(lubridate::now())
  from <- structure(x, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_warning(p <- to_epockperiod(from = from))
  expect_equal(p, c(x, n))
})

test_that("If both are not `NULL`, to_epochperiod returns the period between `from` and `to`.", {
  x <- 1496500000L
  y <- 1496500100L
  from <- structure(x, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  to <- structure(y, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_equal(to_epockperiod(from = from, to = to), c(x, y))
})

