#' S4 class for Calm Results
#' @rdname CalmResult
#' @section Slots:
#' \describe{
#' \item{aggregated_results}{A list with fully aggregated results.}
#' \item{parsed_results}{A list with parsed results, i.e., fn(model_output).}
#' \item{raw_results}{A list with model outputs.}
#' }
#' @exportClass CalmResult
#' @seealso CalmResults-methods
methods::setClass(
  "CalmResult",
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

methods::setClass("CalmExperimentResult",
  contains = "CalmResult"
)

#' Methods for CalmResult
#' @param object A CalmResult object
#' @rdname CalmResult-methods
#' @export
methods::setMethod("show", "CalmResult", function(object) {
  if (!is.null(object@aggregated_results)) {
    print(object@aggregated_results)
  } else {
    print(object@raw_results)
  }
})
