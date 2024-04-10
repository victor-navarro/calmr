#' Get basic designs
#'
#' @param design_name A string specifying a design name (default = NULL)
#' @return If design_name is not NULL, a data.frame containing the design.
#' Otherwise, a list containing all available designs.
#' @seealso [parse_design()]
#' @export
#' @examples
#' names(get_design())
#' get_design("blocking")
get_design <- function(design_name = NULL) {
  calmr_designs <- list(
    "acquisition" = data.frame(
      Group = "Acquisition",
      P1 = "10A>(US)",
      R1 = FALSE
    ),
    "blocking" = data.frame(
      Group = c("Blocking", "Control"),
      P1 = c("10A>(US)", ""),
      R1 = FALSE,
      P2 = c("10AB>(US)/10#B", "10AB>(US)/10#B"),
      R2 = FALSE
    ),
    "relative_validity" = data.frame(
      Group = c("True", "Pseudo"),
      P1 = c("10AB>(US)/10>AC", "5AB>(US)/5AB/5AC>(US)/5AC"),
      R1 = c(TRUE, TRUE),
      P2 = c("1#A", "1#A"),
      R2 = c(TRUE, TRUE)
    ),
    "controlled_blocking" = data.frame(
      Group = c("Blocking", "Control"),
      P1 = c("10A>(US)", "10C>(US)"),
      R1 = FALSE,
      P2 = c("10AB>(US)/10#B", "10AB>(US)/10#B"),
      R2 = FALSE
    )
  )
  if (is.null(design_name)) {
    return(calmr_designs)
  } else {
    calmr_designs[[design_name]]
  }
}
