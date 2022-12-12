#' @title Run a model (quickly)
#' @description Runs the model with minimal parameters.
#' @param design_df A data.frame of dimensions G,2*P+1; where G is the number of groups and P is the number of phases.
#' @param param_df A data.frame of dimensions N,2; where N is the number of stimuli in the experimental design.
#' @param options A list of options, as returned by `get_model_opts`.
#' @param parse A logical specifying whether the results should be parsed via `parse_experiment_results`. Default = TRUE.
#' @return A list with parsed results or a tibble with raw results
#' @seealso \code{\link{get_model_opts}}, \code{\link{parse_experiment_results}}
#' @export
quick_model <- function(design_df, model = NULL, param_df = NULL, options = NULL, parse = TRUE){
  #parse design
  parsed_design = parse_design(design_df)

  #check if parameters were passed
  if (is.null(model)){
    model = "HD2022"
    warning("No model supplied. Using HD2022.")
  }else{
    .calmr_check("supported_model", given = model)
  }

  auto_params = get_params(parsed_design, model = model) #generate parameters

  if (is.null(param_df)){
    param_df = auto_params
    warning("No parameters supplied, using default values.")
  }else{
    #check if the user-specified parameters match what the parser sees
    .calmr_check("params_OK", given = param_df, necessary = auto_params)
  }
  #check if options were passed
  auto_opts = get_model_opts()
  if (is.null(options)){
    options = auto_opts
  }else{
    #check if the user covered all the options requested
    .calmr_check("options_OK", given = options, necessary = auto_opts)
  }

  #make the tibble
  args = make_model_args(design = parsed_design, pars = param_df, model = model, opts = options)
  #run the model
  results = run_model(args, parse = parse)
  return(results)
}
