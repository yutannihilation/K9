#' Get Events
#'
#' This end point allows you to query for event details.
#'
#' @param event_id Event ID.
#' @param start POSIX timestamp.
#' @param end POSIX timestamp.
#' @param priority Priority of events. `NULL`, `"low"` or `"normal"`.
#' @param sources Sources of events. A `character` vector or a single comma-separated `character`.
#' @param tags Tags of events. A named `list` or a single comma-separated `character`.
#' @param .split_request if `TRUE`, automatically split the request when the target period is longer than a day
#'
#' @examples
#' \dontrun{
#' # by default get all events happend from an hour ago
#' k9_get_events()
#'
#' # get all events happend in this week
#' k9_get_events(start = Sys.Date() - 7, end = Sys.Date())
#'
#' # specify an event by ID
#' k9_get_events(event_id = "112233445566")
#'
#' # specify tag
#' k9_get_events(tags = list(role = "db"))
#' }
#'
#' @seealso
#' <http://docs.datadoghq.com/api/?lang=console#events>
#'
#' @export
k9_get_events <- function(event_id = NULL,
                      start = NULL,
                      end = NULL,
                      priority = NULL,
                      sources = NULL,
                      tags = NULL,
                      .split_request = TRUE) {

  if(!is.null(event_id)) {
    if(is.numeric(event_id)) {
      warning('Consider specifying event_id by character, as they might be too big for integer')
    }
    result <- k9_request(verb = "GET",
                         path = sprintf("/api/v1/events/%s", event_id))
  } else {
    # end and start
    period <- to_epochperiod(start, end)

    # priority
    if(!is.null(priority) && !priority %in% c('low', 'normal')) {
      stop("priority can be eighter NULL, 'low', or 'normal'.")
    }

    # sources
    if(!is.null(sources)) {
      sources <- paste(sources, collapse = ",")
    }

    # tags
    if(!is.null(tags) && is.list(tags)) {
      tags <- paste(names(tags), tags, sep = ":", collapse = ",")
    }

    result <- purrr::map2_df(
      .x = dplyr::lag(period)[-1],
      .y = period[-1],
      .f = k9_get_events_one,
      sources = sources,
      tags = tags
    )
  }

  result
}


k9_get_events_one <- function(start, end,
                          priority = NULL,
                          sources = NULL,
                          tags = NULL) {

  result <- k9_request(verb = "GET",
                       path = "/api/v1/events",
                       query = list(
                         start  = start,
                         end    = end,
                         priority = priority,
                         sources = sources,
                         tags = tags
                       ))

  # TODO: convert to a data.frame
  result
}
