#' S4 class for calmr Models
#'
#' @section Slots:
#' \describe{
#' \item{model}{A model name string}
#' \item{parameters}{A list with the model with model parameters}
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
    .last_results = "list"
  )
)

#' CalmrModel methods
#' @description S4 methods for `CalmrModel` class.
#' @param object A [CalmrModel] object.
#' @name CalmrModel-methods
NULL
#> NULL

#' @noRd
setGeneric("run", function(object, ...) standardGeneric("run")) # nocov

#' @aliases run
#' @description Run a [CalmrModel] object with the given parameters,
#' experience, and mapping.
#'
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment()`.
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment()`.
#' @param ... Additional named arguments.
#' @return `run()` returns the updated [CalmrModel] object.
#' @note The `run` method changes some internal
#' states of the model (if appropriate) and
#' populates the `.last_results` slot with the results of the run.
#' @rdname CalmrModel-methods
#' @export
setMethod(
  "run", "CalmrModel",
  function(object, experience, mapping, ...) {
    stop("`run` method not implemented for this model")
  }
)

#' @export
#' @return `parameters()` returns the parameters of the [CalmrModel] object.
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
#' @return `parameters()<-` sets the parameters of a [CalmrModel] object.
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
#' @return results() returns the last results of the [CalmrModel] object.
#' @rdname CalmrModel-methods
methods::setMethod(
  "results", "CalmrModel",
  function(object) {
    if (length(object@.last_results) == 0) {
      stop("No results available. Run the model first.")
    }
    object@.last_results
  }
)

#' @export
#' @return show() returns NULL (invisibly).
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
