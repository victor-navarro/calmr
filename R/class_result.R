#' S4 class for  Calmr parsed_results
#'
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

# could be better
methods::setMethod(
  "c", "CalmrExperimentResult",
  function(x, ..., recursive = FALSE) {
    allres <- list(x, ...)
    h <- methods::new("CalmrExperimentResult")
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
