#' @title Run experiment
#' @description Runs an experiment with minimal parameters.
#' @param x A [CalmrExperiment-class] or design `data.frame`
#' @param outputs A character vector specifying which outputs to
#' parse and aggregate. Defaults to NULL, in which case
#' all model outputs are parsed/aggregated.
#' @param parse A logical specifying whether the raw results
#' should be parsed. Default = TRUE.
#' @param aggregate A logical specifying whether the parsed results
#' should be aggregated. Default = TRUE.
#' @param ... Arguments passed to other functions
#' @return A [CalmrExperiment-class] with results.
#' @examples
#' # Using a data.frame only (throws warning)
#' df <- get_design("relative_validity")
#' run_experiment(df, model = "RW1972")
#'
#' # Using custom parameters
#' df <- get_design("relative_validity")
#' pars <- get_parameters(df, model = "HD2022")
#' pars$alphas["US"] <- 0.6
#' run_experiment(df, parameters = pars, model = "HD2022")
#'
#' # Using make_experiment, for more iterations
#' df <- get_design("blocking")
#' pars <- get_parameters(df, model = "SM2007")
#' exper <- make_experiment(df,
#'   parameters = pars, model = "SM2007",
#'   iterations = 4
#' )
#' run_experiment(exper)
#'
#' # Only parsing the associations in the model, without aggregation
#' run_experiment(exper, outputs = "associations", aggregate = FALSE)
#' @export

run_experiment <- function(
    x, outputs = NULL,
    parse = TRUE,
    aggregate = TRUE, ...) {
  nargs <- list(...)
  if (!is_experiment(x)) {
    # make the experiment
    experiment <- make_experiment(
      design = x, ...
    )
  } else {
    experiment <- x
  }
  # sanitize nargs
  nargs <- nargs[!(names(nargs) %in% c(
    "parameters", "timings",
    "experience", "mapping"
  ))]
  # sanitize outputs using the first model
  outputs <- .sanitize_outputs(outputs, experiment@models[[1]]@outputs)

  # check if experiment needs (can) to be run
  .assert_experiment(experiment)

  # check model/experiment parameters
  experiment <- .assert_model_exp_parameters(experiment)

  # now run the experiment
  exp_length <- length(experiment)
  pb <- progressr::progressor(exp_length)
  .parallel_standby(pb) # print parallel backend message
  # get results
  experiment@models <- future.apply::future_sapply(
    seq_len(exp_length), function(i) {
      if (!is.null(nargs$.callback_fn)) nargs$.callback_fn() # nocov
      # bundle arguments
      args <- c(list(
        object = experiment@models[[i]],
        experience = experiment@experiences[[i]],
        mapping = experiment@design@mapping,
        timings = experiment@timings
      ), nargs)

      mod <- do.call(calmr::run, args)

      # parse results
      if (parse) {
        mod <- parse(mod, outputs)
      }
      pb(message = "Running experiment")
      mod
    },
    simplify = FALSE,
    future.seed = TRUE
  )
  # aggregate
  if (aggregate) {
    experiment <- aggregate(experiment, outputs = outputs)
  }

  return(experiment)
}

.assert_model_exp_parameters <- function(experiment) {
  # here we check whether the model parameters match
  # the ones set in the experiment

  # if the user modifies the parameters after the experiment is created,
  # then the parameters will not match

  # the parameters are named after the group names
  for (i in seq_len(length(experiment))) {
    epars <- experiment@parameters[[experiment@.groups[i]]]
    mpars <- experiment@models[[i]]@parameters
    matches <- mapply(function(a, b) all(a %in% b), epars, mpars)
    if (!all(matches)) {
      experiment@models[[i]]@parameters <- epars
    }
  }
  experiment
}
