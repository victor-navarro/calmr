#' @title Run experiment
#' @description Runs an experiment with minimal parameters.
#' @param x A CalmExperiment or design data.frame
#' @param parse A logical specifying whether the raw results
#' should be parsed. Default = TRUE.
#' @param aggregate A logical specifying whether the parsed results
#' should be aggregated. Default = TRUE.
#' @param ... Arguments passed to other functions
#' @return A CalmExperiment with results.
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
#' # Using CalmExperiment, for more iterations
#' df <- get_design("blocking")
#' pars <- get_parameters(df, model = "SM2007")
#' exper <- make_experiment(df,
#'   parameters = pars, model = "SM2007",
#'   options = get_exp_opts(iterations = 4)
#' )
#' run_experiment(exper)
#' @seealso \code{\link{get_exp_opts}}
#' @export

run_experiment <- function(
    x, parse = TRUE, aggregate = TRUE, ...) {
  nargs <- list(...)
  # start parallel cluster if required
  if (!is_experiment(x)) {
    # parse design
    parsed_design <- parse_design(x)
    # make the experiment
    experiment <- make_experiment(
      design = parsed_design, ...
    )
  } else {
    experiment <- x
  }
  # check if experiment needs (can) to be run
  .calm_assert("good_experiment", given = experiment)

  # now run the experiment
  pb <- progressr::progressor(length(experiment))
  .parallel_standby(pb) # print parallel backend message
  # get results
  all_results <- future.apply::future_apply(
    experiment@arguments, 1, function(i) {
      if (!is.null(nargs$.callback_fn)) nargs$.callback_fn()
      raw <- do.call(get_model(i$model), c(i, ...)) # for shiny
      parsed <- NULL
      if (parse) {
        parsed <- .parse_model(raw = raw, args = i)
      }
      pb(message = "Running experiment")
      list(raw = raw, parsed = parsed)
    },
    simplify = FALSE,
    future.seed = TRUE
  )
  experiment@results@raw_results <- lapply(all_results, "[[", "raw")
  if (parse) {
    experiment@results@parsed_results <- lapply(all_results, "[[", "parsed")
  }
  # aggregate
  if (aggregate) {
    experiment <- aggregate(experiment)
  }

  return(experiment)
}
