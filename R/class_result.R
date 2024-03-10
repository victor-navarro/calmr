#' S4 class for Calmr Results
#' @rdname CalmrResult
#' @section Slots:
#' \describe{
#' \item{aggregated_results}{A list with fully aggregated results.}
#' \item{parsed_results}{A list with parsed results, i.e., fn(model_output).}
#' \item{raw_results}{A list with model outputs.}
#' }
#' @exportClass CalmrResult
#' @seealso CalmrResults-methods
methods::setClass(
  "CalmrResult",
  representation(
    aggregated_results = "list",
    parsed_results = "list",
    raw_results = "list"
  ),
  prototype(
    aggregated_results = NULL,
    parsed_results = NULL,
    raw_results = NULL
  )
)

methods::setClass("CalmrExperimentResult",
  contains = "CalmrResult"
)

#' Methods for CalmrResult
#' @param object A CalmrResult object
#' @rdname CalmrResult-methods
#' @export
methods::setMethod("show", "CalmrResult", function(object) {
  if (!is.null(object@aggregated_results)) {
    message(paste0(capture.output(object@aggregated_results), collapse = "\n"))
  } else {
    message(paste0(capture.output(object@raw_results), collapse = "\n"))
  }
})
