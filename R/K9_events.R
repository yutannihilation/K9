#' Get Events
#'
#' This end point allows you to query for event details.
#'
#' @param event_id Event ID
#' @param start POSIX timestamp
#' @param end POSIX timestamp
#' @param priority `low` or `normal`
#' @param sources A comma separated string of sources
#' @param tags A comma separated string of tags
#'
#' @seealso \url{http://docs.datadoghq.com/api/?lang=console#events}
#'
#' @export
k9_events <- function(event_id = NULL,
                      start = NULL,
                      end = NULL,
                      priority = c('low', 'normal'),
                      sources = NULL,
                      tags = NULL) {

  if(!is.null(event_id)) {
    result <- k9_request(verb = "GET",
                         path = sprintf("/api/v1/events/%d", event_id))
  } else {
    end <- to_epochtime(end)
    if(is.null(start)) {
      start <- end - 3600
    } else {
      start <- to_epochtime(start)
    }

    priority <- match.arg(priority)

    result <- k9_request(verb = "GET",
                         path = "/api/v1/events",
                         query = list(
                           start  = start,
                           end    = end,
                           priority = priority,
                           sources = sources,
                           tags = tags
                         ))
  }

  result
}
