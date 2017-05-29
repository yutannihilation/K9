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
#'
#' @examples
#' \dontrun{
#' # by default get all events happend from an hour ago
#' k9_events()
#'
#' # get all events happend in this week
#' k9_events(start = Sys.Date() - 7, end = Sys.Date())
#'
#' # specify an event by ID
#' k9_events(event_id = "112233445566")
#'
#' # specify tag
#' k9_events(tags = list(role = "db"))
#' }
#'
#' @seealso
#' <http://docs.datadoghq.com/api/?lang=console#events>
#'
#' @export
k9_events <- function(event_id = NULL,
                      start = NULL,
                      end = NULL,
                      priority = NULL,
                      sources = NULL,
                      tags = NULL) {

  if(!is.null(event_id)) {
    if(is.numeric(event_id)) {
      warning('Consider specifying event_id by character, as they might be too big for integer')
    }
    result <- k9_request(verb = "GET",
                         path = sprintf("/api/v1/events/%s", event_id))
  } else {
    # end and start
    end <- to_epochtime(end)
    if(is.null(start)) {
      start <- end - 3600
    } else {
      start <- to_epochtime(start)
    }

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
