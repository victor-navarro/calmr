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
  methods::new("CalmrExperiment",
    arguments = dplyr::bind_rows(lapply(allexps, function(e) e@arguments)),
    design = allexps[[1]]@design,
    results = do.call(c, lapply(allexps, function(e) e@results))
  )
})

#' Extract aggregated results from CalmrExperiment
#'
#' @param object An object of class \clode{\link{CalmrExperiment}}
#' @return A tbl containing models (rows) and model outputs (columns)
#' @export
#' @rdname results
methods::setGeneric("results", function(object) {
  methods::standardGeneric("results")
})
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

# mdat think this will break the parse
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

#' Extract aggregated results from CalmrExperiment
#'
#' @param object An object of class \clode{\link{CalmrExperiment}}
#' @return A tbl containing models (rows) and model outputs (columns)
#' @export
#' @rdname results
methods::setGeneric("results", function(object) {
  methods::standardGeneric("results")
})
methods::setMethod("results", "CalmrExperiment", function(object) {
  # Returns aggregated results
  object@results@aggregated_results
})


#' @export
#' @rdname graph
setMethod("graph", "CalmrExperiment", function(x, ...) {
  if (is.null(x@results@aggregated_results)) {
    stop("Experiment does not contain aggregated results.
      Please parse and aggregate results beforehand.")
  }
  # get aggregated results
  res <- results(x)
  graphs <- list()
  models <- unique(res$model)
  for (m in models) {
    mdat <- res[res$model == m, ]
    assoc_output <- .model_associations(mdat$model)
    weights <- mdat[[assoc_output]][[1]]
    if (assoc_output == c("eivs")) {
      evs <- weights[weights$type == "evs", ]
      ivs <- weights[weights$type == "ivs", ]
      weights <- evs
      weights$value <- weights$value - ivs$value
    }
    groups <- unique(weights$group)
    mgraphs <- list()
    for (g in groups) {
      graph_name <- sprintf("%s - Associations (%s)", g, m)
      mgraphs[[graph_name]] <- calmr_model_graph(
        weights[weights$group == g, ], ...
      ) + ggplot2::labs(title = graph_name)
    }
    graphs[[m]] <- mgraphs
  }
  graphs
})


# Set generic here for rsa function (see rsa.R)
methods::setGeneric("rsa", function(x, layers, ...) standardGeneric("rsa"))

#' Perform representational similarity analysis on CalmrExperiment
#'
#' @param x A tbl of m by o (models by outputs) with aggregated results.
#' @param comparisons A model-named list containing the model
#' outputs to compare.
#' For example, `list("RW1972" = c("vs", "rs"), "HDI2020" = c("as", "vs"))`.
#' @param test Whether to test the RSA via permutation test. Defaults = FALSE.
#' @param ... Additional parameters passed to `stats::dist`
#' and `stats::cor`
#' @returns A CalmrRSA object
#' @note The object returned by this function
#' can be later tested via its own `test` method.
#' @rdname rsa
#' @export
#' @examples
#' # Comparing the associations in three models
#' exp <- data.frame(
#'   Group = c("A", "B"),
#'   P1 = c("2(A)>(US)/1B>(US)", "1(A)>(US)/2B>(US)"),
#'   R1 = TRUE
#' )
#' exp <- parse_design(exp)
#' models <- c("HD2022", "RW1972", "PKH1982")
#' parameters <- sapply(models, get_parameters, design = exp)
#' options <- get_exp_opts()
#' exp_res <- compare_models(exp,
#'   models = models,
#'   parameters = parameters, options = options
#' )
#' comparisons <- list(
#'   "HD2022" = c("vs"),
#'   "RW1972" = c("vs"),
#'   "PKH1982" = c("eivs")
#' )
#' res <- rsa(exp_res, comparisons = comparisons)
#' test(res)
methods::setMethod(
  "rsa", "CalmrExperiment",
  function(x, comparisons, test = FALSE, ...) {
    .rsa(results(x), comparisons = comparisons, .test = test, ...)
  }
)
