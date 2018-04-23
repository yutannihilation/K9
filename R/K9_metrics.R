#' Get List Of Active Metrics
#'
#' Get the list of actively reporting metrics from a given time until now.
#'
#' @param from seconds since the unix epoch
#'
#' @examples
#' \dontrun{
#' # by default, list all metrics available since 1 hour ago
#' k9_list_metrics()
#'
#' # if from argument is provided, this tries to get active metrics from the time
#' k9_list_metrics(Sys.Date() - 1)
#' }
#'
#' @export
k9_list_metrics <- function(from = NULL) {

  if(is.null(from)) from <- Sys.time() - 3600
  from <- to_epochtime(from)

  result <- k9_request(verb = "GET",
                       path = "/api/v1/metrics",
                       query = list(
                         from = from
                       ))

  flatten_list_metrics(result)
}

flatten_list_metrics <- function(result) {
  purrr::flatten_chr(result$metrics)
}

#' Query Time Series Points
#'
#' This end point allows you to query for metrics from any time period.
#'
#' @param query query string
#' @param metric metric name
#' @param scope list of scopes (`scope`)
#' @param by key to group aggregation
#' @param from seconds since the unix epoch
#' @param to seconds since the unix epoch
#' @param .split_request if `TRUE`, automatically split the request when the target period is longer than a day
#'
#' @details
#' You can query either `query`, or the combination of `metric`, `scope` and `by`.
#' For example, on the one hand you can directly query by using
#' `query = "system.cpu.idle{role:db,environment:test}by{host,region}"`.
#' On the other hand, you can specify `metric = "system.cpu.idle"`,
#' `scope = list(role = "db", environment = "test")` and `by = c("host", "region"),`
#' to build the same query.
#'
#' Note that, if `query` is given, the latter will be ignored.
#'
#' `from` and `by` can be one of these:
#' * `numeric`
#' * `POSIXct`
#' * `POSIXlt`
#' * `Date`
#' * `character` (parsed by [anytime::anytime()])
#' * `NULL` (the current epochtime will be used instead)
#'
#' @seealso
#' <http://docs.datadoghq.com/api/?lang=console#metrics>
#' <http://docs.datadoghq.com/graphing/>
#'
#' @export
k9_get_metrics <- function(query = NULL,
                           metric = NULL,
                           scope = NULL,
                           by = NULL,
                           from = NULL, to = NULL,
                           .split_request = TRUE) {

  if(is.null(query)) {
    if(is.null(metric)) stop("please specify query or metric")

    query <- build_query(metric, scope, by)
    message("query: ", query)
  }


  period <- to_epochperiod(from, to, .split_request = .split_request)

  purrr::map2_df(.x = dplyr::lag(period)[-1],
                 .y = period[-1],
                 .f = k9_get_metrics_one,
                 query = query)
}

k9_get_metrics_one <- function(query, from, to) {
  result <- k9_request(verb = "GET",
                       path = "/api/v1/query",
                       query = list(
                         from  = from,
                         to    = to,
                         query = query
                       ))

  result_df <- flatten_get_metrics(result)

  extract_scope(result_df)
}

#' Posts a metric value to Datadog
#'
#' This end point allows you to post time-series data that can be graphed on Datadog’s dashboards or queried from any time period.
#'
#' @param metric the name of the time series
#' @param metric_type type of your metric either: gauge, rate, or count. Optional, default=gauge
#' @param value the numeric value to post
#' @param tags a list of tags associated with the metric.
#' @param interval if the type of the metric is rate or count, define the corresponding interval. Optional, default=None
#'
#' @details
#' The Datadog API uses resource-oriented URLs, uses status codes to indicate the success or failure of requests and returns JSON from all requests. 
#' With this method you can post counters, gauges to measure the value of a 
#' particular thing over time and rates that represent the derivative of a 
#' metric, it’s the value variation of a metric on a defined time interval.
#' @seealso
#' <http://docs.datadoghq.com/api/?lang=console#metrics>
#' <http://docs.datadoghq.com/graphing/>
#' <https://docs.datadoghq.com/developers/metrics/#metric-types> 
#'
#' @export
k9_post_metric <- function(metric, metric_type, value, tags=list(), interval=NULL) {
  series <- list(
      metric = metric,
      type = metric_type,
      points = list(I(c(to_epochtime(Sys.time()),value)))
    )
  if (length(tags) > 0) {
    series["tags"] <- I(tags)
  }
  if (!is.null(interval)) {
    series["interval"] <- c(interval)
  }

  result <- k9_request(verb = "POST",
                        path = "/api/v1/series",
                        body = list("series" = list(series)),
                        encode = "json")

  result
}

flatten_get_metrics <- function(result) {
  purrr::map_df(result$series, flatten_metric_one)
}

# map_int(x, length)
# #>       metric  query_index   attributes display_name         unit    pointlist          end     interval        start       length
# #>            1            1            0            1            2           31            1            1            1            1
# #>         aggr        scope   expression
# #>            0            1            1
flatten_metric_one <- function(x) {
  x_trans <- purrr::transpose(x$pointlist)

  timestamp_epoch <- purrr::flatten_dbl(x_trans[[1]]) / 1000
  timestamp <- anytime::anytime(timestamp_epoch)

  value <- purrr::map_if(x_trans[[2]], is.null, ~ NA)
  value <-  purrr::flatten_dbl(value)

  tibble::tibble(
    timestamp    = timestamp,
    value        = value,
    metric       = x$metric,
    display_name = x$display_name,
    query_index  = x$query_index %||% NA_integer_,
    interval     = x$interval,
    scope        = x$scope,
    expression   = x$expression
  )
}


build_query <- function(metric, scope = NULL, by = NULL) {
  if(is.null(scope)) {
    scope_flatten <- "*"
  } else {
    scope_flatten <- paste(names(scope), scope, sep = ":", collapse = ",")
  }

  if(is.null(by)) {
    as.character(glue::glue("{metric}{{{scope_flatten}}}"))
  } else {
    by_flatten <- paste(by, collapse = ",")
    as.character(glue::glue("{metric}{{{scope_flatten}}}by{{{by_flatten}}}"))
  }
}


extract_scope <- function(df) {
  if(is.null(df) || nrow(df) == 0) {
    warning("df is empty")
    return(df)
  }
  if(!"scope" %in% colnames(df)) {
    warning("df has no scope column; skip extraction")
    return(df)
  }

  scope_example <- df$scope[1]

  regex <- stringr::str_replace_all(scope_example, ":([[:alnum:]_\\-./]+)", ":([[:alnum:]_\\\\-./]+)")
  into <- stringr::str_extract_all(scope_example, "([[:alnum:]_\\-./]+)(?=:)")[[1]]

  # TODO: remove this workaround if tidyr gets .data
  scope <- NULL
  tidyr::extract(df, scope, into, regex)
}
