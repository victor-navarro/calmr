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
    "blocking" = data.frame(
      Group = c("Blocking", "Control"),
      P1 = c("10N>(US)", ""),
      P2 = c("10NL>(US)/10#L", "10NL>(US)/10#L")
    ),
    "relative_validity" = data.frame(
      Group = c("True", "Pseudo"),
      P1 = c("!10AB(US)/10AC", "!5AB(US)/5AB/5AC(US)/5AC"),
      P2 = c("1#A", "1#A")
    ),
    "controlled_blocking" = data.frame(
      Group = c("Blocking", "Control"),
      P1 = c("10N>(US)", "10C>(US)"),
      P2 = c("10NL>(US)/10#L", "10NL>(US)/10#L")
    )
  )
  if (is.null(design_name)) {
    return(calmr_designs)
  } else {
    calmr_designs[[design_name]]
  }
}
