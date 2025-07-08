#' Launch the Shiny App
#'
#' This function launches the Shiny app included in the package.
#' A toy data set to play with is available at 
#' \url{https://www.uv.es/vivigui/docs/data_dashboard.xlsx}.
#'
#' @importFrom shiny runApp
#'
#' @export
run_app_recruit <- function() {
  appDir <- system.file("shinyapp", package = "fawir")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  runApp(appDir, display.mode = "normal")
}
