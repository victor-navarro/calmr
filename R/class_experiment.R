#' S4 class for calm experiments.
#' @section Slots:
#' \describe{
#' \item{\code{arguments}:}{A list containing arguments to run models.}
#' \item{\code{design}:}{A CalmDesign object.}
#' \item{\code{results}:}{A CalmExperimentResult object.}
#' }
#' @rdname CalmExperiment
#' @exportClass CalmExperiment
#' @seealso CalmExperiment-methods

methods::setClass(
  "CalmExperiment",
  representation(
    design = "CalmDesign",
    model = "character",
    groups = "character",
    parameters = "list",
    experiences = "list",
    results = "CalmExperimentResult",
    .model = "character",
    .group = "character",
    .iter = "integer"
  )
)

show <- methods::show
#' @title CalmExperiment methods
#' @rdname CalmExperiment-methods
#' @export
setMethod("show", "CalmExperiment", function(object) {
  cat("-----------------------------\n")
  cat("CalmExperiment with model:\n")
  cat(object@model, "\n")
  cat("-----------------------------\n")
  cat("For design:\n")
  print(object@design@raw_design)
  cat("-----------------------------\n")
  cat("With parameters:\n")
  print(object@parameters)
})

methods::setGeneric("design", function(x) methods::standardGeneric("design"))
#' @export
#' @aliases design
#' @rdname CalmExperiment-methods
methods::setMethod("design", "CalmExperiment", function(x) {
  x@design
})

#' @export
#' @aliases trials
#' @rdname CalmExperiment-methods
methods::setMethod("trials", "CalmExperiment", function(object) {
  trials(object@design)
})

methods::setGeneric("parameters", function(x) standardGeneric("parameters"))
#' @rdname CalmExperiment-methods
#' @aliases parameters
#' @export
methods::setGeneric(
  "parameters<-",
  function(x, value) standardGeneric("parameters<-")
)
#' @rdname CalmExperiment-methods
#' @param x A CalmExperiment
#' @aliases parameters
#' @export
methods::setMethod(
  "parameters", "CalmExperiment",
  function(x) x@parameters
)
#' @param x A CalmExperiment
#' @param value A list of parameter lists.
#' @rdname CalmExperiment-methods
#' @export
methods::setMethod("parameters<-", "CalmExperiment", function(x, value) {
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

methods::setGeneric("experiences", function(x) standardGeneric("experiences"))
#' @rdname CalmExperiment-methods
#' @aliases experience
#' @export
methods::setMethod(
  "experiences", "CalmExperiment",
  function(x) x@experiences
)

methods::setGeneric(
  "results",
  function(object) methods::standardGeneric("results")
)
#' @rdname CalmExperiment-methods
#' @aliases results
#' @export
methods::setMethod("results", "CalmExperiment", function(object) {
  # Returns aggregated results
  object@results@aggregated_results
})

methods::setGeneric(
  "raw_results",
  function(object) methods::standardGeneric("raw_results")
)
#' @rdname CalmExperiment-methods
#' @aliases raw_results
#' @export
methods::setMethod("raw_results", "CalmExperiment", function(object) {
  # Returns raw results
  object@results@raw_results
})

methods::setGeneric(
  "parsed_results",
  function(object) methods::standardGeneric("parsed_results")
)
#' @rdname CalmExperiment-methods
#' @aliases parsed_results
#' @export
methods::setMethod("parsed_results", "CalmExperiment", function(object) {
  # Returns raw results
  object@results@parsed_results
})

#' @rdname CalmExperiment-methods
#' @export
methods::setMethod("length", "CalmExperiment", function(x) {
  length(x@experiences)
})

#' @rdname CalmExperiment-methods
#' @aliases parse
#' @export
setGeneric(
  "parse",
  function(object) methods::standardGeneric("parse")
)
#' @rdname CalmExperiment-methods
#' @export
methods::setMethod(
  "parse", "CalmExperiment",
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

#' @rdname CalmExperiment-methods
#' @export
methods::setMethod(
  "aggregate", "CalmExperiment",
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

setGeneric("plot", function(x, y, ...) methods::standardGeneric("plot"))

#' Plot CalmExperiment
#'
#' Creates plots (or plot) with aggregated results in CalmExperiment
#'
#' @param x An object of class \code{\link{CalmExperiment-class}}.
#' @param type character vector specifying the types of plots to create.
#' See ??supported_plots. Defaults to NULL.
#' @return A ggplot object
#' @note With type = NULL, all supported plots are returned.
#' @export
#' @aliases plot
#' @rdname CalmExperiment-methods
setMethod(
  "plot", "CalmExperiment",
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
        sapply(type, .calm_assert, supported = model_plots)
        model_plots <- type
      }
      for (p in model_plots) {
        odat <- res[[p]]
        pdat <- odat[odat$model == m, ]
        groups <- unique(pdat$group)
        for (g in groups) {
          plot_name <- sprintf("%s - %s (%s)", g, .get_prettyname(p), m)
          plots[[plot_name]] <- calm_model_plot(pdat[pdat$group == g, ],
            type = p
          )
        }
      }
    }
    plots
  }
)

setGeneric("graph", function(x, ...) standardGeneric("graph"))
#' Graph CalmExperiment
#' @param x A CalmExperiment
#' @param ... Additional parameters passed to `graph_calm_model`
#' @export
#' @rdname graph
setMethod("graph", "CalmExperiment", function(x, ...) {
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
      mgraphs[[graph_name]] <- calm_model_graph(
        weights[weights$group == g, ], ...
      ) + ggplot2::labs(title = graph_name)
    }
    graphs[[m]] <- mgraphs
  }
  graphs
})
