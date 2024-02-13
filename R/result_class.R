#' S4 class for  Calmr parsed_results
#'
#' @section Slots:
#' \describe{
#' \item{\code{parsed_results}:}{Data.frame or List. The unparsed model parsed_results}
#' \item{\code{parsed_results}:}{List. The parsed and aggregated model parsed_results}
#' \item{\code{is_parsed}:}{Logical. Whether the model parsed_results have been parsed}
#' }
#' @name CalmrResult
#' @rdname CalmrResult
#' @exportClass CalmrResult
#' @import tibble

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

# Method to print parsed_results
methods::setMethod("show", "CalmrResult", function(object) {
  if (!is.null(object@aggregated_results)) {
    print(object@aggregated_results)
  } else {
    print(object@raw_results)
  }
})

methods::setClass("CalmrExperimentResult",
  contains = "CalmrResult"
)
