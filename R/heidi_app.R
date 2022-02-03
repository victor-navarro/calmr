#' Run the heidi GUI
#' @description Starts a shiny application
#' @param browser A logical specifying whether the app should be launched into a browser. Default = TRUE.
#' @export

heidi_app <- function(browser = TRUE) {
  app_dir = system.file("heidi_app", package = "heidi")
  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing `heidi`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = browser)
}
