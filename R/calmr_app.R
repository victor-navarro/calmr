#' Run the calmr GUI
#' @description Starts a shiny application
#' @param browser If TRUE, the app is launched in a browser.
#' @export

calmr_app <- function(browser = getOption("shiny.launch.browser", interactive())) {
  app_dir = system.file("calmr_app", package = "calmr")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `calmr`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
