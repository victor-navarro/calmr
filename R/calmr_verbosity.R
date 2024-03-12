#' Set verbosity options for calmr
#' @description Whether to show verbosity messages and progress bars
#' @param verbose A logical
#' @note Progress bars are handled by the progressr package.
#' This is just a convenience function.
#' @export
#' @return The list of progressr handlers (invisibly).
#' @note See package 'progressr' for further details.
calmr_verbosity <- function(verbose) {
  if (verbose) {
    progressr::handlers("progress")
    progressr::handlers(global = TRUE)
  } else {
    progressr::handlers("void")
    progressr::handlers(global = FALSE)
  }
}
