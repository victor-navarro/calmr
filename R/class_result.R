#' S4 class for calmr results
#' @rdname CalmrResult
#' @section Slots:
#' \describe{
#' \item{aggregated_results}{A list of `data.table` objects
#' with aggregated results.}
#' \item{parsed_results}{A list containing `data.table` objects
#' with parsed results.}
#' \item{raw_results}{A list with raw model outputs.}
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
#' S4 class for calmr experiment results
#' @rdname CalmrExperimentResult
#' @section Slots:
#' \describe{
#' \item{aggregated_results}{A list of `data.table` objects
#' with aggregated results.}
#' \item{parsed_results}{A list containing `data.table` objects
#' with parsed results.}
#' \item{raw_results}{A list with raw model outputs.}
#' }
#' @exportClass CalmrExperimentResult
methods::setClass("CalmrExperimentResult",
  contains = "CalmrResult"
)
#' CalmrResult methods
#' @description S4 methods for `CalmrResults` class.
#' @param object A `CalmrResults` object.
#' @name CalmrResult-methods
#' @returns
#' * `show()` returns NULL (invisibly).
NULL
#> NULL

#' @rdname CalmrResult-methods
#' @export
methods::setMethod("show", "CalmrResult", function(object) {
  if (!is.null(object@aggregated_results)) {
    message(paste0(utils::capture.output(object@aggregated_results),
      collapse = "\n"
    ))
  } else {
    message(paste0(utils::capture.output(object@raw_results), collapse = "\n"))
  }
})
