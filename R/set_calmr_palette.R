#' Get/set the colour/fill palette for plots
#' @param palette A string specifying the available palettes.
#' If NULL, returns available palettes.
#' @return The old palette (invisibly) if palette is not NULL.
#' Otherwise, a character vector of available palettes.
#' @note Changes here do not affect the palette used in graphs.
#' @export

set_calmr_palette <- function(palette = NULL) {
  avail_palettes <- c("viridis", "hue")
  if (is.null(palette)) {
    return(avail_palettes)
  }
  palette <- .assert_valid_palette(palette)
  old <- options("calmr_palette" = palette)
  invisible(old)
}
