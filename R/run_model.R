#' @title Run a model
#' @description Runs a model with specific arguments.
#' @param args A tibble as returned by `make_model_args`.
#' @param parse A logical specifying whether the results should be parsed. Default = TRUE.
#' @return A HeidiExperiment object.
#' @seealso \code{\link{make_model_args}}
#' @export

#TODO: Break down the contents of the HeidiExperiment object
run_model <- function(args, parse = TRUE){
  ex = new("HeidiExperiment",
           results = args)
  ex@results$mod_data = apply(args, 1, function(x) do.call(get_model(x[[1]]), x[-1:-3]))
  if (parse){
    ex = parse_experiment_results(ex)
  }
  ex
}
