#' S4 class for calmr Models
#'
#' @section Slots:
#' \describe{
#' \item{model_name}{A model name string}
#' \item{outputs}{A character vector with model outputs}
#' \item{parameters}{A list with the model with model parameters}
#' \item{default_parameters}{A list with the default model parameters}
#' \item{.internal_states}{A character vector with internal states}
#' \item{.is_timed}{A logical indicating if the model is timed}
#' \item{.associations}{A character vector with associations output name}
#' \item{.dnames_map}{A list with data names mapping for outputs}
#' \item{.parse_map}{A list with parse functions for outputs}
#' \item{.formula_map}{A list with formula mapping for outputs}
#' \item{.plots_map}{A list with plot functions for outputs}
#' \item{.last_experience}{A data.frame with the last experience run}
#' \item{.last_raw_results}{A list with the last raw results}
#' \item{.last_parsed_results}{A list with the last parsed results}
#' }
#' @name CalmrModel-class
#' @rdname CalmrModel-class
#' @exportClass CalmrModel
methods::setClass(
  "CalmrModel",
  representation(
    model_name = "character",
    outputs = "character",
    parameters = "list",
    default_parameters = "list",
    .internal_states = "character",
    .is_timed = "logical",
    .associations = "character",
    .dnames_map = "list",
    .parse_map = "list",
    .formula_map = "list",
    .plots_map = "list",
    .last_experience = "data.frame",
    .last_raw_results = "list",
    .last_parsed_results = "list"
  )
)

#' CalmrModel methods
#' @description S4 methods for [CalmrModel-class]
#' @param object A [CalmrModel-class] object.
#' @name CalmrModel-methods
NULL
#> NULL

#' @noRd
setGeneric("run", function(object, ...) standardGeneric("run")) # nocov

#' @aliases run
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment()`.
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment()`.
#' @param timings A named list specifying timings for the model. Only used
#' for timed models.
#' @param ... Additional named arguments.
#' @return `run()` returns the [CalmrModel-class] after
#' running the phases in the design.
#' @note The `run` method changes some internal
#' states of the model (if appropriate) and
#' populates the `.last_raw_results` slot with the results of the run.
#' @rdname CalmrModel-methods
#' @export
setMethod(
  "run", "CalmrModel",
  function(object, experience, mapping, timings, ...) {
    stop("`run` method not implemented for this model")
  }
)

#' @export
#' @param x A [CalmrModel-class] object.
#' @return `parameters()` returns the parameters
#' of the [CalmrModel-class] object.
#' @rdname CalmrModel-methods
methods::setMethod(
  "parameters", "CalmrModel",
  function(x) {
    if (is.null(x@parameters)) {
      stop("Model parameters are not set. Use `parameters<-` to set them.")
    }
    x@parameters
  }
)

#' @export
#' @param value A list of parameters to set.
#' @return `parameters()<-` sets the parameters of a [CalmrModel-class] object.
#' @rdname CalmrModel-methods
methods::setMethod(
  "parameters<-", "CalmrModel",
  function(x, value) {
    # assert the value is a list
    if (!is.list(value)) {
      stop("Parameters must be a list.")
    }
    # assert the value has the same names as the default parameters
    if (!all(names(value) %in% x@default_parameters$name)) {
      stop("Parameters must match the default parameters.")
    }
    x@parameters <- value
    x
  }
)

#' @export
#' @return `raw_results()` returns the last
#' raw results of the [CalmrModel-class] object.
#' @rdname CalmrModel-methods
methods::setMethod(
  "raw_results", "CalmrModel",
  function(object) {
    if (length(object@.last_raw_results) == 0) {
      stop("No raw results available. Run the model first.")
    }
    object@.last_raw_results
  }
)

#' @export
#' @return `parsed_results()` returns the last
#' parsed results of the [CalmrModel-class] object.
#' @rdname CalmrModel-methods
methods::setMethod(
  "parsed_results", "CalmrModel",
  function(object) {
    if (length(object@.last_parsed_results) == 0) {
      stop("No parsed results available. Parse the model first.")
    }
    object@.last_parsed_results
  }
)

#' @export
#' @return `show()` returns NULL (invisibly).
#' @rdname CalmrModel-methods
methods::setMethod(
  "show", "CalmrModel",
  function(object) {
    if (length(parameters(object)) > 0) {
      par_text <- c(
        "-----------------------------\n",
        "Parameters:\n",
        paste0(utils::capture.output(object@parameters),
          collapse = "\n"
        )
      )
    } else {
      par_text <- c(
        "-----------------------------\n",
        "Default Parameters:\n",
        paste0(utils::capture.output(object@default_parameters),
          collapse = "\n"
        )
      )
    }
    message(
      paste0("CalmrModel (", object@model_name, ")\n"),
      par_text,
      "-----------------------------\n",
      "\nOutputs: ",
      paste(object@outputs, collapse = ", "),
      "\n"
    )
  }
)

#' @export
#' @param outputs A character vector specifying the outputs to parse.
#' If not specified, all outputs of the model will be parsed.
#' @return `parse()` returns [CalmrModel-class] with parsed results.
#' @rdname CalmrModel-methods
methods::setMethod(
  "parse", "CalmrModel",
  function(object, outputs = object@outputs) {
    if (length(object@.last_raw_results) == 0) {
      stop("No results available. Run the model first.")
    }
    if (!all(outputs %in% object@outputs)) {
      stop("Requested outputs are not available in the model.")
    }
    # parse results
    parsed_results <- lapply(outputs, function(o) {
      if (is.null(object@.parse_map[[o]])) {
        stop(paste("No parse function defined for output:", o))
      }
      object@.parse_map[[o]](object, o)
    })
    names(parsed_results) <- outputs
    object@.last_parsed_results <- parsed_results
    object
  }
)

#' @export
#' @param type A character vector specifying the
#' types of plots to generate (should be model outputs).
#' @return `plot()` returns a list of 'ggplot' plot objects.
#' @rdname CalmrModel-methods
setMethod(
  "plot", "CalmrModel",
  function(x, type = NULL, ...) {
    # Assert type is valid
    model_plots <- .sanitize_outputs(type, x@outputs)
    # Check parsed results are available
    res <- parsed_results(x)
    if (length(res) == 0) {
      stop(c(
        "Experiment must have parsed results. ",
        "Use `parse` on your model first."
      ))
    }
    # assert outputs are in aggregated results
    stopifnot(
      "Some outputs are not available in aggregated results.
      Use aggregate on your experiment." =
        all(model_plots %in% names(res))
    )

    # make plots
    plots <- list()
    for (p in model_plots) {
      odat <- res[[p]]
      group <- unique(odat$group)
      plot_name <- sprintf(
        "%s - %s (%s)", group,
        .get_y_prettyname(p), x@model_name
      )
      # get plot
      plots[[plot_name]] <- x@.plots_map[[p]](odat, ...) +
        ggplot2::labs(title = plot_name)
    }
    plots
  }
)


#' @rdname CalmrModel-methods
#' @return `graph()` returns a 'ggplot' object.
#' @export
setMethod("graph", "CalmrModel", function(x, ...) {
  # get parsed results
  res <- parsed_results(x)
  if (length(res) == 0) {
    stop("Model does not contain parsed results.
      Please parse model beforehand.")
  }
  weights <- res[[x@.associations]]
  if (x@model_name == "PKH1982") {
    evs <- weights[weights$type == "EV", ]
    ivs <- weights[weights$type == "IV", ]
    weights <- evs
    weights$value <- weights$value - ivs$value
  }
  group <- unique(weights$group)
  graph_name <- sprintf("%s - Associations (%s)", group, x@model_name)
  g <- calmr_model_graph(
    weights[weights$group == group, ], ...
  ) + ggplot2::labs(title = graph_name)
  g
})
