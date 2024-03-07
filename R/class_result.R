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
#' @import tibble
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


#' @param x A CalmResult object
#' @param ... More CalmResult objects to concatenate
#' @param recursive Unused
#' @rdname CalmResult-methods
#' @export
methods::setMethod(
  "c", "CalmExperimentResult",
  function(x, ..., recursive = FALSE) {
    allres <- list(x, ...)
    h <- methods::new("CalmExperimentResult")
    raws <- do.call(c, lapply(
      allres, function(e) e@raw_results
    ))
    if (!is.null(raws)) {
      h@raw_results <- raws
    }
    pars <- do.call(c, lapply(
      allres, function(e) e@parsed_results
    ))
    if (!is.null(pars)) {
      h@parsed_results <- pars
    }
    if (!is.null(x@aggregated_results)) {
      allouts <- unique(unlist(lapply(
        allres, function(e) names(e@aggregated_results)
      )))
      aggs <- sapply(allouts, function(o) {
        dplyr::bind_rows(lapply(allres, function(r) {
          r@aggregated_results[[o]]
        }))
      }, simplify = FALSE)
      h@aggregated_results <- aggs
    }
    return(h)
  }
)
