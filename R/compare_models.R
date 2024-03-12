#' @title Run models given a set of parameters
#' @param x A list of [CalmrExperiment-class] objects or a design [data.frame].
#' @param models A character vector of length m, specifying the models to run.
#' Ignored if x is a list of [CalmrExperiment-class] objects.
#' @param ... Arguments passed to [make_experiment].
#' @return A list of [CalmrExperiment-class] objects
#' @export
#' @examples
#' # By making experiment beforehand (recommended)
#' df <- get_design("blocking")
#' models <- c("HD2022", "RW1972", "PKH1982")
#' exps <- lapply(models, function(m) {
#'   make_experiment(df,
#'     parameters = get_parameters(df, model = m),
#'     model = m
#'   )
#' })
#' comp <- compare_models(exps)
#'
#' # By passing minimal arguments (not recommended; default parameters)
#' comp <- compare_models(df, models = models)
compare_models <- function(
    x,
    models = NULL,
    ...) {
  if (is_design(x)) stop("Please pass an unparsed data.frame.")
  if (all(sapply(x, is_experiment))) {
    all_exps <- x
  } else {
    # assert models
    if (!is.null(models)) {
      models <- sapply(models, .calmr_assert, what = "supported_model")
      stopifnot(
        "Argument `models` must contain unique model names." =
          length(models) == length(unique(models))
      )
    } else {
      stop("If x is a design data.frame, you must pass `models`")
    }
    # make the arguments
    all_exps <- lapply(models, function(m) {
      make_experiment(x,
        parameters = get_parameters(x, model = m),
        model = m, ...
      )
    })
  }
  all_exps <- lapply(all_exps, run_experiment)
  names(all_exps) <- sapply(all_exps, "slot", "model")
  all_exps
}
