#' @title Run experiment
#' @description Runs an experiment with minimal parameters.
#' @param design A data.frame of dimensions G,2*P+1; where G is the number of groups and P is the number of phases.
#' @param model A string specifying the model name. One of `supported_models()`
#' @param param_df A data.frame of dimensions N,2; where N is the number of stimuli in the experimental design.
#' @param options A list of options, as returned by `get_exp_opts`.
#' @param parse A logical specifying whether the results should be parsed via `parse_experiment_results`. Default = TRUE.
#' @return A list with parsed results or a tibble with raw results
#' @seealso \code{\link{get_exp_opts}}, \code{\link{parse_experiment_results}}
#' @export
run_experiment <- function(design, model = NULL, param_df = NULL, options = NULL, parse = TRUE){
  #parse design
  parsed_design = parse_design(design)

  #check if parameters were passed
  if (is.null(model)){
    model = .calmr_default("model_name")
  }else{
    .calmr_check("supported_model", given = model)
  }

  if (is.null(param_df)){
    param_df = .calmr_default("model_params", design = parsed_design, model = model)
  }else{
    #check if the user-specified parameters match what the parser sees
    auto_params = get_model_params(design = parsed_design, model = model)
    .calmr_check("params_OK", given = param_df, necessary = auto_params)
  }
  #check if options were passed
  if (is.null(options)){
    options = .calmr_default("experiment_options")
  }else{
    #check if the user covered all the options requested
    auto_opts = get_exp_opts()
    .calmr_check("options_OK", given = options, necessary = auto_opts)
  }

  #make the tibble
  args = make_model_args(design = parsed_design, pars = param_df, model = model, opts = options)

  #check if experiment needs to be run
  .calmr_check("good_experiment", given = args)

  #run the model
  results = run_model(args, parse = parse)
  return(results)
}
