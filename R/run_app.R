#' Launch the Uno Shiny dashboard
#'
#' Opens the interactive Uno app in the default browser (or the RStudio
#' Viewer). The app provides three tabs:
#' \itemize{
#'   \item \strong{Play} — interactive Uno against 1-3 bot opponents
#'   \item \strong{Simulate} — run thousands of games via the Rcpp engine
#'   \item \strong{Leaderboard} — persistent session leaderboard
#' }
#'
#' @param port port to listen on (default: a random available port)
#' @param launch.browser passed to \code{shiny::runApp}
#' @return called for its side-effect
#' @export
run_app <- function(port = NULL, launch.browser = TRUE) {
  app_dir <- system.file("app", package = "uno")
  if (!nzchar(app_dir)) {
    stop("Could not find the Shiny app directory inside the uno package.")
  }
  args <- list(appDir = app_dir, launch.browser = launch.browser)
  if (!is.null(port)) args$port <- port
  do.call(shiny::runApp, args)
}
