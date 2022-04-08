#' Run the heidi GUI
#' @description Starts a shiny application
#' @param browser If TRUE, the app is launched in a browser.
#' @export

heidi_app <- function(browser = getOption("shiny.launch.browser", interactive())) {
  app_dir = system.file("heidi_app", package = "heidi")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `heidi`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
