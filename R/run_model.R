#' @title Run a model
#' @description Runs a model with specific arguments.
#' @param args A tibble as returned by `make_model_args`.
#' @param parse A logical specifying whether the results should be parsed. Default = TRUE.
#' @return A list with parsed results or a tibble with raw results
#' @seealso \code{\link{make_model_args}}
#' @export
run_model <- function(args, parse = TRUE){
  results = args
  results$mod_data = apply(args, 1, function(x) do.call(get_model(x[[1]]), x[-1:-3]))
  if (parse){
    results = parse_experiment_results(results)
  }
  results
}
