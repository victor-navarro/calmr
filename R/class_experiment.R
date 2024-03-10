#' S4 class for calmr experiments.
#' @section Slots:
#' \describe{
#' \item{\code{design}:}{A CalmrDesign object.}
#' \item{\code{model}:}{A string specifying the model used.}
#' \item{\code{groups}:}{A string specifying the groups in the design.}
#' \item{\code{parameters}:}{A list with the parameters used, per group.}
#' \item{\code{experiences}:}{A list with the experiences for the model.}
#' \item{\code{results}:}{A CalmrExperimentResult object.}
#' \item{\code{.model}:}{Internal. The model associated with the iteration.}
#' \item{\code{.group}:}{Internal. The group associated with the iteration.}
#' \item{\code{.iter}:}{Internal. The iteration number.}
#' }
#' @rdname CalmrExperiment
#' @exportClass CalmrExperiment
#' @seealso CalmrExperiment-methods

methods::setClass(
  "CalmrExperiment",
  representation(
    design = "CalmrDesign",
    model = "character",
    groups = "character",
    parameters = "list",
    experiences = "list",
    results = "CalmrExperimentResult",
    .model = "character",
    .group = "character",
    .iter = "integer"
  )
)

show <- methods::show
#' @title CalmrExperiment methods
#' @param object A CalmrExperiment object
#' @rdname CalmrExperiment-methods
#' @export
setMethod("show", "CalmrExperiment", function(object) {
  message(
    "-----------------------------\n",
    "CalmrExperiment with model:\n",
    object@model, "\n",
    "-----------------------------\n",
    "Design:\n",
    paste0(capture.output(object@design@raw_design), collapse = "\n"),
    "\n",
    "-----------------------------\n",
    "Parameters:\n",
    paste0(capture.output(object@parameters), collapse = "\n")
  )
})

methods::setGeneric( # nocov start
  "design",
  function(x) methods::standardGeneric("design")
) # nocov end
#' @export
#' @aliases design
#' @rdname CalmrExperiment-methods
methods::setMethod("design", "CalmrExperiment", function(x) {
  x@design
})

#' @export
#' @rdname CalmrExperiment-methods
methods::setMethod("trials", "CalmrExperiment", function(object) {
  trials(object@design)
})

methods::setGeneric(
  "parameters",
  function(x) standardGeneric("parameters")
) # nocov
#' @rdname CalmrExperiment-methods
#' @aliases parameters
#' @export
methods::setGeneric(
  "parameters<-",
  function(x, value) standardGeneric("parameters<-") # nocov
)
#' @rdname CalmrExperiment-methods
#' @param x A CalmrExperiment
#' @aliases parameters
#' @export
methods::setMethod(
  "parameters", "CalmrExperiment",
  function(x) x@parameters
)
#' @param x A CalmrExperiment
#' @param value A list of parameter lists.
#' @rdname CalmrExperiment-methods
#' @export
methods::setMethod("parameters<-", "CalmrExperiment", function(x, value) {
  newpars <- NULL
  oldpars <- parameters(x)
  gnames <- names(oldpars)
  parnames <- unlist(lapply(oldpars, names))
  valnames <- names(value)
  # Check if the user passed group-level parameters
  if (length(setdiff(gnames, valnames))) {
    # If not, check if the user passed appropriate
    # parameters (from get_parameters)
    if (!length(setdiff(parnames, valnames))) {
      newpars <- lapply(oldpars, function(g) {
        value
      })
    }
  } else {
    valparnames <- unlist(lapply(value, names))

    if (!length(setdiff(parnames, valparnames)) &&
      (length(parnames) == length(valparnames))) {
      newpars <- value
    }
  }
  if (is.null(newpars)) {
    stop(paste(
      "Could not find a match for group/parameter names.",
      "Try calling parameters on experiment before trying the assignment."
    ))
  }
  x@parameters <- newpars
  x
})

methods::setGeneric(
  "experiences",
  function(x) standardGeneric("experiences") # nocov
)
#' @rdname CalmrExperiment-methods
#' @aliases experiences
#' @export
methods::setMethod(
  "experiences", "CalmrExperiment",
  function(x) x@experiences
)

methods::setGeneric(
  "results",
  function(object) methods::standardGeneric("results") # nocov
)
#' @rdname CalmrExperiment-methods
#' @aliases results
#' @export
methods::setMethod("results", "CalmrExperiment", function(object) {
  # Returns aggregated results
  object@results@aggregated_results
})

methods::setGeneric(
  "raw_results",
  function(object) methods::standardGeneric("raw_results") # nocov
)
#' @rdname CalmrExperiment-methods
#' @aliases raw_results
#' @export
methods::setMethod("raw_results", "CalmrExperiment", function(object) {
  # Returns raw results
  object@results@raw_results
})

methods::setGeneric(
  "parsed_results",
  function(object) methods::standardGeneric("parsed_results") # nocov
)
#' @rdname CalmrExperiment-methods
#' @aliases parsed_results
#' @export
methods::setMethod("parsed_results", "CalmrExperiment", function(object) {
  # Returns raw results
  object@results@parsed_results
})

#' @rdname CalmrExperiment-methods
#' @export
methods::setMethod("length", "CalmrExperiment", function(x) {
  length(x@experiences)
})

#' @rdname CalmrExperiment-methods
#' @aliases parse
#' @export
setGeneric( # nocov start
  "parse",
  function(object) methods::standardGeneric("parse")
) # nocov end
#' @rdname CalmrExperiment-methods
#' @export
methods::setMethod(
  "parse", "CalmrExperiment",
  function(object) {
    if (!is.null(object@results@raw_results)) {
      # we gotta parse
      n <- length(object)
      pb <- progressr::progressor(n)
      .parallel_standby(pb) # print parallel backend message
      object@results@parsed_results <- future.apply::future_sapply(
        seq_len(n), function(r) {
          pb("Parsing results")
          .parse_model(
            raw = object@results@raw_results[[r]],
            experience = object@experiences[[r]],
            model = object@model
          )
        },
        simplify = FALSE
      )
    } else {
      stop("Found no raw_results to parse.")
    }
    object
  }
)

#' @rdname CalmrExperiment-methods
#' @param ... Extra parameters.
#' @export
methods::setMethod(
  "aggregate", "CalmrExperiment",
  function(x, ...) {
    if (is.null(x@results@parsed_results)) {
      x <- parse(x)
    }
    res <- .aggregate_experiment(x, ...)
    # unnest_once to leave at output level
    x@results@aggregated_results <- unlist(unname(res),
      recursive = FALSE
    )
    x
  }
)


setGeneric("plot", function(x, y, ...) methods::standardGeneric("plot")) # nocov
#' Plot CalmrExperiment
#'
#' Creates plots (or plot) with aggregated results in CalmrExperiment
#'
#' @param x An object of class \code{\link{CalmrExperiment-class}}.
#' @param type character vector specifying the types of plots to create.
#' See ??supported_plots. Defaults to NULL.
#' @return A ggplot object
#' @note With type = NULL, all supported plots are returned.
#' @export
#' @aliases plot
#' @rdname CalmrExperiment-methods
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
    models <- unique(x@model)
    # Go through each row
    for (m in models) {
      model_plots <- supported_plots(m)
      if (!is.null(type)) {
        sapply(type, .calmr_assert, supported = model_plots)
        model_plots <- type
      }
      for (p in model_plots) {
        odat <- res[[p]]
        pdat <- odat[odat$model == m, ]
        groups <- unique(pdat$group)
        for (g in groups) {
          plot_name <- sprintf("%s - %s (%s)", g, .get_prettyname(p), m)
          plots[[plot_name]] <- calmr_model_plot(pdat[pdat$group == g, ],
            type = p
          )
        }
      }
    }
    plots
  }
)
#' Graph CalmrExperiment
#' @param x A CalmrExperiment
#' @param ... Additional parameters passed to `graph_calmr_model`
#' @rdname CalmrExperiment-methods
setGeneric("graph", function(x, ...) standardGeneric("graph")) # nocov
#' @rdname CalmrExperiment-methods
#' @aliases graph,CalmrExperiment
#' @export
setMethod("graph", "CalmrExperiment", function(x, ...) {
  if (is.null(x@results@aggregated_results)) {
    stop("Experiment does not contain aggregated results.
      Please parse and aggregate results beforehand.")
  }
  # get aggregated results
  res <- results(x)
  graphs <- list()
  models <- unique(x@model)
  for (m in models) {
    assoc_output <- .model_associations(m)
    odat <- res[[assoc_output]]
    weights <- odat[odat$model == m, ]
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
