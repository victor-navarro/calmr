#' @title Run experiment
#' @description Runs an experiment with minimal parameters.
#' @param design A data.frame of dimensions G,2*P+1;
#' where G is the number of groups and P is the number of phases.
#' @param model A string specifying the model name. One of `supported_models()`
#' @param param_df A data.frame of dimensions N,2; where N
#' is the number of stimuli in the experimental design.
#' @param options A list of options, as returned by `get_exp_opts`.
#' @param parse A logical specifying whether the results
#' should be parsed via `parse_experiment_results`. Default = TRUE.
#' @param ... Extra parameters passed to the model call (e.g., debug)
#' @return A list with parsed results or a tibble with raw results
#' @seealso \code{\link{get_exp_opts}}, \code{\link{parse_experiment_results}}
#' @export

run_experiment <- function(
    design, model = NULL,
    param_df = NULL, options = NULL, parse = TRUE, ...) {
  # parse design
  parsed_design <- parse_design(design)

  # check if parameters were passed
  model <- .calmr_assert("supported_model", model)

  param_df <- .calmr_assert("parameters", design = design, model = model)
  # check if options were passed
  options <- .calmr_assert("experiment_options", options)

  # make the tibble
  args <- make_model_args(
    design = parsed_design,
    pars = param_df, model = model, opts = options
  )

  # check if experiment needs (can) to be run
  .calmr_assert("good_experiment", given = args)

  # run the model
  run_model(args, parse = parse, ...)
}
