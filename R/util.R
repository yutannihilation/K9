# util

k9_request <- function(verb, path, query = NULL, ...) {
  api_key <- Sys.getenv("DATADOG_API_KEY")
  application_key <- Sys.getenv("DATADOG_APP_KEY")

  if(api_key == "" || application_key == "") {
    stop("run k9_auth() first.")
  }

  if(is.list(query)) {
    query <- purrr::update_list(query,
                                api_key = api_key,
                                application_key = application_key)
  } else {
    warning(sprintf("Ignoring invalid query: %s", query))
    query <- list(api_key = api_key,
                  application_key = application_key)
  }

  res <- httr::VERB(
    verb = verb,
    url = "https://app.datadoghq.com",
    path = path,
    query = query,
    ...
  )

  httr::stop_for_status(res)

  x_ratelimit_remaining <- res$headers[["x-ratelimit-remaining"]]
  x_ratelimit_reset <- res$headers[["x-ratelimit-reset"]]

  if (!is.null(x_ratelimit_remaining)) {
    message(sprintf("This API is rate-limited and you have %s requests left (reset after %s seconds)\n",
                    x_ratelimit_remaining, x_ratelimit_reset))
  }

  httr::content(res)
}

to_epochtime <- function(x) {
  purrr::when(x,
              is.numeric(.) ~ as.integer(.),
              lubridate::is.POSIXct(.) ~ as.integer(.),
              lubridate::is.POSIXlt(.) ~ as.integer(as.POSIXct(.)),
              lubridate::is.Date(.) ~ as.integer(as.POSIXct(.)),
              is.character(.) ~ anytime::anytime(.),
              is.null(.) ~ as.integer(Sys.time() - lubridate::minutes(10)))
}
