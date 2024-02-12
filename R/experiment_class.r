#' CalmrExperiment
#' @description S4 classes for calmr experiments.
#' @section Slots:
#' \describe{
#' \item{\code{.Data}:}{Inherited from tbl class}
#' \item{\code{raw_results}:}{List. The raw results.}
#' }
#' @name CalmrExperiment
#' @rdname CalmrExperiment
#' @exportClass CalmrExperiment

methods::setClass(
  "CalmrExperiment",
  representation(
    arguments = "tbl",
    results = "CalmrResult"
  )
)

setMethod("show", "CalmrExperiment", function(object) {
  summary(object)
})

#' Summarise CalmrExperiment
#'
#' Shows a summary for a CalmrExperiment
#'
#' @param object An object of class \code{\link{CalmrExperiment}}.
#' @param ... Additional parameters passed to the summary function.
#' @return A
#' @export
setMethod("summary", "CalmrExperiment", function(object, ...) {
  print("TODO")
  # print(object@results$mod_data[[1]])
  # outputs <- names(object@results$mod_data[[1]]@model_results) # works for parsed and non-parsed objects
  # cat("Available model outputs:\n")
  # cat(outputs, "\n\n")
  # cat(sprintf("Use `get_output(model, type)` to access the outputs (e.g., `get_output(%s, '%s')`)", as.character(substitute(object)), outputs[1]), "\n")
})

#' Plot CalmrExperiment
#'
#' Creates a plot depicting results from CalmrExperiment
#'
#' @param x An object of class \code{\link{CalmrExperiment-class}}.
#' @param type A string specifying the type of plot to create.
#' See ??supported_plots.
#' @param ... Additional parameters passed to the plotting function.
#' @return A ggplot object
#' @export
#' @rdname plot

setMethod(
  "plot", "CalmrExperiment",
  function(x, type = NULL, y = NULL, ...) {
    if (is.null(type)) {
      type <- "vs"
    }
    # parse if model has not been parsed
    if (!x@is_parsed) {
      x <- parse_experiment_results(x)
    }

    .calmr_assert("supported_plot", type,
      supported = names(x@parsed_results)
    )
    plotinfo <- .get_plot_functions(type)

    plotf <- plotinfo[[type]]$fun
    if (type %in% c("evs", "ivs")) {
      dat <- rbind(
        data.frame(x@parsed_results[["evs"]], assoc_type = "Excitatory"),
        data.frame(x@parsed_results[["ivs"]], assoc_type = "Inhibitory")
      )
    } else {
      dat <- x@parsed_results[[type]]
    }

    groups <- unique(dat$group)
    ps <- sapply(groups, function(g) {
      plotf(dat[dat$group == g, ], ...) +
        ggplot2::labs(title = sprintf("Group = %s", g))
    }, simplify = F)
    names(ps) <- groups
    ps
  }
)

# #' Get output from CalmrExperiment
# #'
# #' Returns a tibble containing parsed outputs of CalmrExperiment
# #'
# #' @param object An object of class \code{\link{CalmrExperiment-class}}.
# #' @param type The type of output
# #' @return A data.frame with the model output
# #' @export

# get_output <- function(object, type = NULL) NULL
# setMethod("get_output", "CalmrExperiment", function(object, type = NULL) {
#   if (is.null(type)) {
#     outputs <- names(object@results$mod_data[[1]]@model_results) # works for parsed and non-parsed objects
#     cat("Available model outputs:\n")
#     cat(outputs, "\n\n")
#     cat(sprintf("Use `get_output(model, type)` to access the outputs (e.g., `get_output(%s, '%s')`)", as.character(substitute(object)), outputs[1]), "\n")
#   } else {
#     if (!object@is_parsed) {
#       object <- parse_experiment_results(object)
#     }
#     object@parsed_results[[type]]
#   }
# })

setMethod("graph", "CalmrExperiment", function(x, ...) {
  if (any(c("evs", "ivs") %in% names(x@parsed_results))) {
    dat <- x@parsed_results$evs
    dat$value <- dat$value - x@parsed_results$ivs$value
  } else {
    dat <- x@parsed_results$vs
  }
  groups <- unique(dat$group)
  ps <- sapply(groups, function(g) {
    graph_weights(dat[dat$group == g, ], ...) +
      ggplot2::labs(title = sprintf("Group = %s", g))
  }, simplify = F)
  names(ps) <- groups
  ps
})
