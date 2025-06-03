#' S4 class for calmr experiments.
#' @section Slots:
#' \describe{
#' \item{\code{design}:}{A [CalmrDesign-class] object.}
#' \item{\code{groups}:}{A string specifying the groups in the design.}
#' \item{\code{model}:}{A string specifying the model used.}
#' \item{\code{parameters}:}{A list with the parameters used, per group.}
#' \item{\code{timings}:}{A list with the timings used in the design.}
#' \item{\code{experiences}:}{A list with the experiences for the model.}
#' \item{\code{results}:}{A list with aggregated results.}
#' \item{\code{models}:}{The models associated with the iteration.}
#' \item{\code{.groups}:}{Internal. The groups associated with the iteration.}
#' \item{\code{.iter}:}{Internal. The iteration number.}
#' \item{\code{.seed}:}{The seed used to generate the experiment.}
#' }
#' @rdname CalmrExperiment
#' @exportClass CalmrExperiment
#' @seealso CalmrExperiment-methods

methods::setClass(
  "CalmrExperiment",
  representation(
    design = "CalmrDesign",
    groups = "character",
    model = "character",
    parameters = "list",
    timings = "list",
    experiences = "list",
    results = "list",
    models = "list",
    .groups = "character",
    .iter = "integer",
    .seed = "ANY"
  )
)


#' CalmrExperiment methods
#' @description S4 methods for `CalmrExperiment` class.
#' @param object,x A `CalmrExperiment` object.
#' @param outputs A character vector specifying the model outputs to parse.
#' @param type A character vector specifying the type(s) of plots to create.
#' Defaults to NULL. See [supported_plots].
#' @param value A list of parameters (or list of parameter lists).
#' @param ... Extra arguments passed to [calmr_model_graph()].
#' @name CalmrExperiment-methods
NULL
#> NULL

#' @noRd
show <- methods::show
#' @export
#' @return `show()` returns NULL (invisibly).
#' @rdname CalmrExperiment-methods
setMethod("show", "CalmrExperiment", function(object) {
  message(
    "-----------------------------\n",
    "CalmrExperiment with model:\n",
    class(object@model), "\n",
    "-----------------------------\n",
    "Design:\n",
    paste0(utils::capture.output(object@design@raw_design), collapse = "\n"),
    "\n",
    "-----------------------------\n",
    "Parameters:\n",
    paste0(utils::capture.output(object@parameters), collapse = "\n")
  )
})
#' @noRd
methods::setGeneric( # nocov start
  "design",
  function(x) standardGeneric("design")
) # nocov end
#' @export
#' @return `design()` returns the `CalmrDesign` contained in the object.
#' @aliases design
#' @rdname CalmrExperiment-methods
methods::setMethod("design", "CalmrExperiment", function(x) {
  x@design
})

#' @export
#' @return `trials()` returns NULL (invisibly).
#' @aliases trials
#' @rdname CalmrExperiment-methods
methods::setMethod("trials", "CalmrExperiment", function(object) {
  trials(object@design)
})
#' @noRd
methods::setGeneric(
  "parameters",
  function(x) standardGeneric("parameters")
) # nocov
#' @noRd
methods::setGeneric(
  "parameters<-",
  function(x, value) standardGeneric("parameters<-") # nocov
)
#' @rdname CalmrExperiment-methods
#' @return `parameters()` returns the list of parameters
#' contained in the object.
#' @aliases parameters
#' @export
methods::setMethod(
  "parameters", "CalmrExperiment",
  function(x) x@parameters
)
#' @rdname CalmrExperiment-methods
#' @return `parameters()<-` returns the object after updating parameters.
#' @aliases parameters<-
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
      "Try calling `parameters()` on the experiment before
      attempting the assignment."
    ))
  }
  x@parameters <- newpars

  # update model parameters
  for (i in seq_along(x@models)) {
    x@models[[i]]@parameters <- newpars[[x@.groups[i]]]
  }

  x
})
#' @noRd
methods::setGeneric(
  "experiences",
  function(x) standardGeneric("experiences") # nocov
)
#' @rdname CalmrExperiment-methods
#' @aliases experiences
#' @return `experiences()` returns a list of `data.frame` objects
#' containing model training routines.
#' @export
methods::setMethod(
  "experiences", "CalmrExperiment",
  function(x) x@experiences
)
#' @noRd
methods::setGeneric(
  "experiences<-",
  function(x, value) standardGeneric("experiences<-") # nocov
)
#' @rdname CalmrExperiment-methods
#' @return `experiences()<-` returns the object after updating experiences.
#' @aliases experiences<-
#' @export
methods::setMethod(
  "experiences<-", "CalmrExperiment",
  function(x, value) {
    stopifnot(
      "value must be either 1 or length(experiences(x))" =
        length(value) == 1 || length(value) == length(experiences(x))
    )
    x@experiences <- value
    x
  }
)
#' @noRd
methods::setGeneric(
  "results",
  function(object) standardGeneric("results") # nocov
)
#' @rdname CalmrExperiment-methods
#' @return `results()` returns a `data.table` objects with aggregated results.
#' @aliases results
#' @export
methods::setMethod("results", "CalmrExperiment", function(object) {
  object@results
})
#' @noRd
methods::setGeneric(
  "raw_results",
  function(object) standardGeneric("raw_results") # nocov
)
#' @rdname CalmrExperiment-methods
#' @return `raw_results()` returns a list with raw model results.
#' @aliases raw_results
#' @export
methods::setMethod("raw_results", "CalmrExperiment", function(object) {
  lapply(object@models, function(m) m@.last_raw_results)
})

#' @noRd
methods::setGeneric(
  "parsed_results",
  function(object) standardGeneric("parsed_results") # nocov
)
#' @rdname CalmrExperiment-methods
#' @aliases parsed_results
#' @return `parsed_results()` returns a list of `data.table`
#' objects with parsed results.
#' @export
methods::setMethod("parsed_results", "CalmrExperiment", function(object) {
  lapply(object@models, function(m) m@.last_parsed_results)
})

#' @rdname CalmrExperiment-methods
#' @return `length()` returns an integer specifying the total length
#' of the experiment (groups by iterations).
#' @export
methods::setMethod("length", "CalmrExperiment", function(x) {
  length(x@experiences)
})

#' @noRd
setGeneric( # nocov start
  "parse",
  function(object, ...) standardGeneric("parse")
) # nocov end
#' @rdname CalmrExperiment-methods
#' @return `parse()` returns the object after parsing raw results.
#' @aliases parse
#' @export
methods::setMethod(
  "parse", "CalmrExperiment",
  function(object, outputs = NULL) {
    # Sanitize outputs
    outputs <- .sanitize_outputs(
      outputs,
      object@models[[1]]@outputs
    )

    n <- length(object)
    pb <- progressr::progressor(n)
    .parallel_standby(pb) # print parallel backend message
    object@models <- future.apply::future_sapply(
      seq_len(n), function(r) {
        # parse
        pb("Parsing results")
        parse(object@models[[r]], outputs = outputs)
      },
      simplify = FALSE
    )
    object
  }
)

#' @return `aggregate()` returns the object after aggregating parsed results.
#' @rdname CalmrExperiment-methods
#' @aliases aggregate
#' @export
methods::setMethod(
  "aggregate", "CalmrExperiment",
  function(x, outputs = NULL) {
    outputs <- .sanitize_outputs(outputs, x@models[[1]]@outputs)
    res <- .aggregate_experiment(x, outputs)
    # unnest_once to leave at output level
    x@results <- unlist(unname(res),
      recursive = FALSE
    )
    x
  }
)

setGeneric("plot", function(x, y, ...) standardGeneric("plot")) # nocov
#' @export
#' @return `plot()` returns a list of 'ggplot' plot objects.
#' @aliases plot
#' @rdname CalmrExperiment-methods
setMethod(
  "plot", "CalmrExperiment",
  function(x, type = NULL, ...) {
    # Assert type is valid
    model_plots <- .sanitize_outputs(type, x@models[[1]]@outputs)
    # Check aggregated results are available
    res <- results(x)
    if (length(res) == 0) {
      stop(c(
        "Experiment must have aggregated results. ",
        "Use `aggregate` on your experiment first."
      ))
    }
    # assert outputs are in aggregated results
    stopifnot(
      "Some outputs are not available in aggregated results.
      Use aggregate on your experiment." =
        all(model_plots %in% names(res))
    )

    # get plots from first model
    pfuncs <- x@models[[1]]@.plots_map
    plots <- list()
    for (p in model_plots) {
      odat <- res[[p]]
      pdat <- odat[odat$model == x@model, ]
      groups <- unique(pdat$group)
      for (g in groups) {
        plot_name <- sprintf("%s - %s (%s)", g, .get_y_prettyname(p), x@model)
        # get plot
        plots[[plot_name]] <- pfuncs[[p]](pdat[pdat$group == g, ], ...) +
          ggplot2::labs(title = plot_name) +
          ggplot2::labs(
            y = .get_y_prettyname(p),
            colour = .get_scale_prettyname(p),
            fill = .get_scale_prettyname(p)
          )
      }
    }
    plots
  }
)

#' @noRd
setGeneric("graph", function(x, ...) standardGeneric("graph")) # nocov
#' @rdname CalmrExperiment-methods
#' @aliases graph
#' @return `graph()` returns a list of 'ggplot' plot objects.
#' @export
setMethod("graph", "CalmrExperiment", function(x, ...) {
  # get aggregated results
  res <- results(x)
  if (length(res) == 0) {
    stop("Experiment does not contain aggregated results.
      Please parse and aggregate results beforehand.")
  }
  graphs <- list()
  assoc_output <- x@models[[1]]@.associations
  weights <- res[[assoc_output]]
  if (x@model == "PKH1982") {
    evs <- weights[weights$type == "EV", ]
    ivs <- weights[weights$type == "IV", ]
    weights <- evs
    weights$value <- weights$value - ivs$value
  }
  groups <- unique(weights$group)
  for (g in groups) {
    graph_name <- sprintf("%s - Associations (%s)", g, x@model)
    graphs[[graph_name]] <- calmr_model_graph(
      weights[weights$group == g, ], ...
    ) + ggplot2::labs(title = graph_name)
  }
  graphs
})

#' @noRd
methods::setGeneric(
  "timings",
  function(x) standardGeneric("timings") # nocov
)
#' @noRd
methods::setGeneric(
  "timings<-",
  function(x, value) standardGeneric("timings<-") # nocov
)
#' @rdname CalmrExperiment-methods
#' @return `timings()` returns the list of timings
#' contained in the object.
#' @aliases timings
#' @export
methods::setMethod(
  "timings", "CalmrExperiment",
  function(x) x@timings
)
#' @rdname CalmrExperiment-methods
#' @return `timings()<-` returns the object after updating timings.
#' @aliases timings<-
#' @export
methods::setMethod("timings<-", "CalmrExperiment", function(x, value) {
  .assert_timings(value, design(x), x@model)
  x@timings <- value
  x
})

#' @noRd
setGeneric(
  "filter",
  function(x, ...) standardGeneric("filter") # nolint: line_length_linter.
) # nocov
#' @rdname CalmrExperiment-methods
#' @param trial_types A character vector with trial types to filter.
#' @param phases A character vector with phase names to filter.
#' @param stimuli A character vector with stimulus names to filter.
#' @return `filter()` returns the object after filtering
#' parsed aggregated results
#' @aliases filter
#' @export
methods::setMethod("filter", "CalmrExperiment", function(
    x,
    trial_types = NULL,
    phases = NULL, stimuli = NULL) {
  if (is.null(x@results)) {
    stop(c(
      "Experiment must have aggregated results. ",
      "Use `aggregate` on your experiment first."
    ))
  }
  res <- results(x)
  # filter phases
  if (!is.null(phases)) {
    res <- lapply(
      res,
      function(r) r[r$phase %in% phases, ]
    )
  }
  if (!is.null(trial_types)) {
    res <- lapply(
      res,
      function(r) r[r$trial_type %in% trial_types, ]
    )
  }
  if (!is.null(stimuli)) {
    res <- lapply(
      res,
      function(r) r[r$s1 %in% stimuli, ]
    )
    res <- lapply(
      res,
      function(r) {
        if ("s2" %in% names(r)) {
          r[r$s2 %in% stimuli, ]
        } else {
          r
        }
      }
    )
  }
  x@results <- res
  x
})
