#' @title Run a model (quickly)
#' @description Runs the model with minimal parameters.
#' @param design_df A data.frame of dimensions G,2*P+1; where G is the number of groups and P is the number of phases.
#' @param param_df A data.frame of dimensions N,2; where N is the number of stimuli in the experimental design.
#' @param options A list of options, as returned by `get_model_opts`.
#' @param parse A logical specifying whether the results should be parsed via `parse_experiment_results`. Default = TRUE.
#' @return A list with parsed results or a tibble with raw results
#' @seealso \code{\link{get_model_opts}}, \code{\link{parse_experiment_results}}
#' @export
quick_model <- function(design_df, param_df = NULL, options = NULL, model = NULL, parse = TRUE){
  parsed_design = parse_design(design_df)
  auto_params = get_params(parsed_design) #generate parameters
  #check if parameters were passed
  if (is.null(param_df)){
    param_df = auto_params
    warning(sprintf("No parameters supplied, using default values of %1.2f", default_alpha))
  }else{
    #check if the user-specified parameters match what the parser sees
    if (!(all(auto_params[, 1] %in% param_df[, 1]))){
      stop("Error: Found mismatch between user supplied stimulus names and those in the experimental design.")
    }
  }
  #check if options were passed
  auto_opts = get_model_opts()
  if (is.null(options)){
    options = auto_opts
  }else{
    #check if the user covered all the options requested
    if (any(!(names(auto_opts) %in% names(options)))){
      stop("Error: Did not supply some of the options required to fit the model. Please see ?get_model_opts")
    }
  }
  #check if model was passed
  auto_mod = "HD2022"
  if (is.null(model)){
    model = auto_mod
    warning(sprintf("No model supplied, using %s", auto_mod))
  }

  #make the tibble
  heidi_df = make_model_args(design = parsed_design, pars = param_df, model = model, opts = options)
  #run the model
  results = run_model(heidi_df, parse = parse)
  return(results)
}
