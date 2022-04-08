#' @title Run the HeiDI model (quickly)
#' @description Runs the model with minimal parameters.
#' @param design_df A data.frame of dimensions G,2*P+1; where G is the number of groups and P is the number of phases.
#' @param param_df A data.frame of dimensions N,2; where N is the number of stimuli in the experimental design.
#' @param options A list of options, as returned by `get_heidi_opts`.
#' @param parse A logical specifying whether the results should be parsed via `parse_heidi_results`. Default = TRUE.
#' @return A list with parsed results or a tibble with raw results
#' @seealso \code{\link{get_heidi_opts}}, \code{\link{parse_heidi_results}}
#' @export
quick_heidi <- function(design_df, param_df = NULL, options = NULL, parse = TRUE){
  default_alpha = .2
  parsed_design = parse_design(design_df)
  auto_params = get_params(parsed_design, default_par = default_alpha) #generate parameters
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
  auto_opts = get_heidi_opts()
  if (is.null(options)){
    options = auto_opts
  }else{
    #check if the user covered all the options requested
    if (any(!(names(auto_opts) %in% names(options)))){
      stop("Error: Did not supply some of the options required to fit the model. Please see ?get_heidi_opts")
    }
  }
  #make the tibble
  heidi_df = make_heidi_args(parsed_design, param_df, options)
  #run the model
  results = run_heidi(heidi_df, parse = parse)
  return(results)
}
