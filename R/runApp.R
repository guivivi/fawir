#' Launch the Shiny App
#'
#' This function launches the Shiny app included in the package.
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
