#' Get basic designs
#'
#' @param design_name A string specifying a design name (default = NULL)
#' @returns If design_name != NULL, a data.frame containing the design.
#' Otherwise, a list containing all available designs.
#' @export

get_design <- function(design_name = NULL) {
  calm_designs <- list(
    "blocking" = data.frame(
      Group = c("Blocking", "Control"),
      P1 = c("10N>(US)", ""),
      R1 = FALSE,
      P2 = c("10NL>(US)/10#L", "10NL>(US)/10#L"),
      R2 = FALSE
    ),
    "relative_validity" = data.frame(
      Group = c("True", "Pseudo"),
      P1 = c("10AB(US)/10AC", "5AB(US)/5AB/5AC(US)/5AC"),
      R1 = c(TRUE, TRUE),
      P2 = c("1#A", "1#A"),
      R2 = c(TRUE, TRUE)
    )
  )
  if (is.null(design_name)) {
    return(calm_designs)
  } else {
    calm_designs[[design_name]]
  }
}
