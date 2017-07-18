# util

k9_request <- function(verb, path, query = list(), ...) {
  api_key <- Sys.getenv("DATADOG_API_KEY")
  application_key <- Sys.getenv("DATADOG_APP_KEY")

  if(api_key == "" || application_key == "") {
    stop("run k9_auth() first.")
  }

  if(!is.list(query)) {
    warning(sprintf("Ignoring invalid query: %s", query))
    query <- list()
  }
  query$api_key <- api_key
  query$application_key <- application_key

  res <- httr::VERB(
    verb = verb,
    url = "https://app.datadoghq.com",
    path = path,
    query = query,
    ...
  )

  x_ratelimit_remaining <- res$headers[["x-ratelimit-remaining"]]
  x_ratelimit_reset <- res$headers[["x-ratelimit-reset"]]

  if (!is.null(x_ratelimit_remaining)) {
    message(sprintf("This API is rate-limited and you have %s requests left (reset after %s seconds)\n",
                    x_ratelimit_remaining, x_ratelimit_reset))
  }

  result <- httr::content(res)
  warn_msg <- result$warnings
  err_msg <- result$errors

  if(!is.null(warn_msg)) warning(paste(warn_msg, collapse = "\n"))
  if(!is.null(err_msg))  stop(paste(err_msg, collapse = "\n"))

  # in case the response does not contain "errors" yet the status code shows some error happened...
  httr::stop_for_status(res)

  result
}

to_epochtime <- function(x) {
  if(length(x) > 1) stop('x must be a vector of length one.')

  purrr::when(x,
              is.numeric(.) ~ as.integer(.),
              lubridate::is.POSIXct(.) ~ as.integer(.),
              lubridate::is.POSIXlt(.) ~ as.integer(as.POSIXct(.)),
              lubridate::is.Date(.) ~ as.integer(as.POSIXct(.)),
              is.character(.) ~ as.integer(anytime::anytime(.)),
              ~ stop("Unsupported type", typeof(.)))
}

# 1) If `from` and `to` are both `NULL`, get metrics between 1 day ago and now.
# 2) If only `from` is `NULL`, get metrics between 1 day before from `to`, and `to`.
# 3) If only `to` is `NULL`, get metrics between `from` and now.
# 4) If both are not `NULL`, get metrics between `from` and `to`.
#
# Show a warning if the period is longer than 24 hour since its granularity may be degraded then.
to_epochperiod <- function(from = NULL, to = NULL, .split_request = TRUE) {
  to <- to_epochtime(to %||% Sys.time())
  from <- to_epochtime(from %||% (to - 86400L))

  if(.split_request){
    result <- seq(from, to, by = 86400L)
    # to might be already included in result
    result <- unique(c(result, to))
  } else {
    if(to - from > 86400L) warning("The period is longer than 24 hour; the granularity of metrics may be degraded.")
    result <- c(from, to)
  }

  result
}
