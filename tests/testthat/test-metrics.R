context("test-metrics")

test_that("k9_list_metrics works", {
  result <- jsonlite::read_json(system.file("extdata/list_metrics.json", package = "datadogr"))
  expect_equal(
    flatten_list_metrics(result),
    c("system.load.1", "system.load.15", "system.load.5", "system.load.norm.1",
      "system.load.norm.15", "system.load.norm.5", "system.mem.buffered",
      "system.mem.cached", "system.mem.committed", "system.mem.free")
  )
})

test_that("k9_get_metrics works", {
  result <- jsonlite::read_json(system.file("extdata/get_metrics.json", package = "datadogr"))
  x <- flatten_get_metrics(result)
  expect_equal(
    nrow(x),
    2L
  )
})
