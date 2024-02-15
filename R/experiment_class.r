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
    design = "CalmrDesign",
    results = "CalmrExperimentResult"
  )
)

setMethod("show", "CalmrExperiment", function(object) {
  if (is.null(object@results@raw_results)) {
    print(object@arguments)
  } else {
    print(object@results)
  }
})

methods::setGeneric("design", function(x) methods::standardGeneric("design"))
methods::setMethod("design", "CalmrExperiment", function(x) {
  x@design
})


# A method to concatenate experiments
# TODO: Implement a concatenation methods for CalmrDesign
# This currently deletes some design information if the designs
# are not the same across experiments
methods::setMethod("c", "CalmrExperiment", function(x, ..., recursive = FALSE) {
  allexps <- list(x, ...)
  if (length(allexps) > 1) {
    methods::new("CalmrExperiment",
      arguments = dplyr::bind_rows(lapply(allexps, function(e) e@arguments)),
      design = allexps[[1]]@design,
      results = do.call(c, lapply(allexps, function(e) e@results))
    )
  } else {
    stop("Cannot concatenate with less than 2 experiments.")
  }
})

methods::setGeneric("results", function(object) {
  methods::standardGeneric("results")
})

#' Extract aggregated results from CalmrExperiment
#'
#' @param object An object of class \clode{\link{CalmrExperiment}}
#' @return A tbl containing models (rows) and model outputs (columns)
#' @export
#' @rdname results

setMethod("results", "CalmrExperiment", function(object) {
  # Returns aggregated results
  object@results@aggregated_results
})

methods::setMethod("length", "CalmrExperiment", function(x) {
  if (!is.null(x@arguments)) {
    nrow(x@arguments)
  } else {
    NULL
  }
})

# I think this will break the parse
setGeneric("parse", function(object) methods::standardGeneric("parse"))
methods::setMethod(
  "parse", "CalmrExperiment",
  function(object) {
    if (!is.null(object@results@raw_results)) {
      # we gotta parse
      object@results@parsed_results <- .parse_experiment(object)
    } else {
      stop("Found no raw_results to parse.")
    }
    object
  }
)

methods::setMethod(
  "aggregate", "CalmrExperiment",
  function(x, ...) {
    if (is.null(x@results@parsed_results)) {
      x <- parse(x)
    }
    res <- .aggregate_experiment(x)
    x@results@aggregated_results <- do.call(
      dplyr::bind_rows,
      sapply(names(res), function(m) {
        tibble::tibble(model = m, tibble::as_tibble(lapply(res[[m]], list)))
      }, simplify = FALSE)
    )
    x
  }
)

#' Plot CalmrExperiment
#'
#' Creates plots (or plot) with aggregated results in CalmrExperiment
#'
#' @param x An object of class \code{\link{CalmrExperiment}}.
#' @param type character vector specifying the types of plots to create.
#' See ??supported_plots. Defaults to NULL.
#' @return A ggplot object
#' @note With type = NULL, all supported plots are returned.
#' @export
#' @rdname plot
#'
#'
setGeneric("plot", function(x, y, ...) methods::standardGeneric("plot"))

setMethod(
  "plot", "CalmrExperiment",
  function(x, type = NULL, ...) {
    if (is.null(x@results@aggregated_results)) {
      stop("Experiment does not contain aggregated results.
      Please parse and aggregate results beforehand.")
    }
    # get aggregated results
    res <- results(x)
    plots <- list()
    models <- unique(res$model)
    # Go through each row
    for (m in models) {
      mdat <- res[res$model == m, ]
      model_plots <- supported_plots(m)
      if (!is.null(type)) {
        sapply(type, .calmr_assert, supported = model_plots)
        model_plots <- type
      }
      row_plots <- list()
      for (p in model_plots) {
        pdat <- mdat[[p]][[1]]
        groups <- unique(pdat$group)
        for (g in groups) {
          plot_name <- sprintf("%s - %s (%s)", g, .get_prettyname(p), m)
          row_plots[[plot_name]] <- calmr_model_plot(pdat[pdat$group == g, ],
            type = p
          )
        }
      }
      plots[[m]] <- row_plots
    }
    plots
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
