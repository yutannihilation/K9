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
  purrr::when(x,
              is.numeric(.) ~ as.integer(.),
              lubridate::is.POSIXct(.) ~ as.integer(.),
              lubridate::is.POSIXlt(.) ~ as.integer(as.POSIXct(.)),
              lubridate::is.Date(.) ~ as.integer(as.POSIXct(.)),
              is.character(.) ~ anytime::anytime(.),
              is.null(.) ~ as.integer(Sys.time()))
}
