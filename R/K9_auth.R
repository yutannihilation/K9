#' Authentication for 'Datadog'
#'
#' Set API Key And Application Key
#'
#' @export
k9_auth <- function() {
  Sys.setenv(DATADOG_API_KEY = ask_for_secret("API Key"))
  Sys.setenv(DATADOG_APP_KEY = ask_for_secret("Application Key"))
}

ask_for_secret <- function(prompt) {
  if(rstudioapi::isAvailable()) {
    rstudioapi::askForPassword(prompt)
  } else {
    cat(prompt)
    readLines(n = 1L)
  }
}
