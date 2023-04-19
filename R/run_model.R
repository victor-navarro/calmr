#' @title Run a model
#' @description Runs a model with specific arguments.
#' @param args A tibble as returned by `make_model_args`.
#' @param parse A logical specifying whether the results should be parsed.
#' @return A CalmrExperiment object.
#' @seealso \code{\link{make_model_args}}
#' @export

#TODO: Break down the contents of the CalmrExperiment object
run_model <- function(args, parse = T, ...){
  ex = methods::new("CalmrExperiment",
                    results = args)
  ex@results$mod_data = apply(args, 1, function(x) do.call(what = get_model(x[[1]]),
                                                           args = c(x[-1:-3], ...)))
  if (parse){
    ex = parse_experiment_results(ex)
  }
  ex
}
