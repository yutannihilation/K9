context("extract-scope")

test_that("extract_scope works", {
  d <- tibble::tibble(
    timestamp    = 1496060448L,
    value        = 1.0,
    metric       = "met.ri.c",
    display_name = "metric",
    query_index  = 0L,
    interval     = 30L,
    scope        = "role:db,env:production,type:tidyverse-1",
    expression   = "met.ri.c{role:db,env:production,type:tidyverse-1}"
  )
  de <- extract_scope(d)
  expect_equal(colnames(de), c("timestamp", "value", "metric", "display_name", "query_index",
                               "interval", "role", "env", "type", "expression"))
  expect_equal(de$role[1], "db")
  expect_equal(de$env[1], "production")
  expect_equal(de$type[1], "tidyverse-1")
})

test_that("extract_scope skips data frame without scope column", {
  d <- iris
  testthat::expect_warning(de <- extract_scope(d))
  expect_equal(d, de)
})

test_that("extract_scope skips empty data frame", {
  d <- tibble::tibble()
  testthat::expect_warning(de <- extract_scope(d))
  expect_equal(d, de)
})
