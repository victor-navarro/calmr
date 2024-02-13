#' @title Run experiment
#' @description Runs an experiment with minimal parameters.
#' @param x A CalmrExperiment or design data.frame
#' @param parse A logical specifying whether the results
#' should be parsed. Default = TRUE.
#' @param ... Arguments to make the experiment
#' (in case x is a design data.frame)
#' or arguments for the model call (e.g., debug)
#' @return A CalmrExperiment with results.
#' @examples
#' # Using a data.frame only
#' df <- data.frame(
#'   Group = c("True", "Pseudo"),
#'   P1 = c("10AB(US)/10AC", "5AB(US)/5AB/5AC(US)/5AC"),
#'   R1 = c(TRUE, TRUE),
#'   P2 = c("1A", "1A"),
#'   R2 = c(TRUE, TRUE)
#' )
#' run_experiment(df)
#' run_experiment(model = "MAC1975")
#'
#' # Using a CalmrExperiment with modified parameters
#' df <- data.frame(
#'   Group = c("Weak", "Strong"),
#'   P1 = c("10AB(US)", "10AC(US)"),
#'   R1 = FALSE
#' )
#' pars <- get_parameters(model = "HD2022")
#' exp <- make_experiment(df, parameters = pars)
#' run_experiment(exp)
#' @seealso \code{\link{get_exp_opts}}, \code{\link{parse_experiment_results}}
#' @export

run_experiment <- function(
    x, parse = TRUE, ...) {
  if (!is_experiment(x)) {
    # parse design
    parsed_design <- parse_design(x)
    # make the experiment
    experiment <- make_experiment(
      design = parsed_design, ...
    )
  }
  # check if experiment needs (can) to be run
  .calmr_assert("good_experiment", given = experiment)

  # now run the experiment
  results <- apply(experiment@arguments, 1, function(i) {
    do.call(get_model(i$model), i)
  }, simplify = FALSE)

  experiment@resuls@raw_results <- results
  return(experiment)
}
