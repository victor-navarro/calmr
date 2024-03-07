#' Run the calm shiny application
#' @param browser If TRUE, the app is launched in a browser.
#' calm.app package from github. Defaults to FALSE.
#' @note The shiny app is distributed separately from this package.


app <- function(browser = TRUE, install_pkg = FALSE) {
  if (requireNamespace("calm.app", quietly = TRUE)) {
    calm.app::launch_app(browser = browser)
  } else {
    stop(c(
      "Launching the app requires installation of the calm.app package.\n",
      "Please install the package from github using devtools.\n",
      "hint: `devtools::install_github('victor-navarro/calm.app')`"
    ))
  }
}
