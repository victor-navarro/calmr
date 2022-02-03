#' @title Run the HeiDI model
#' @description Runs the model given a data.frame containing the experimental design and parameters.
#' @param design_df A data.frame of dimensions G,2*P+1; where G is the number of groups and P is the number of phases.
#' @param param_df A data.frame of dimensions N,2; where N is the number of stimuli in the experimental design.
#' @param options A list of options, as returned by getHeidiOpts.
#' @param parse A logical specifying whether the results should be parsed. Default = TRUE.
#' @note
#' The specification of design_df should adhere to the following rules:
#' \itemize{
#' \item{
#' The first column should specify the Group names
#' }
#' \item{
#' The remaining columns should be paired, each pair denoting a single experimental phase.
#' The first containing trial strings (see below) and logicals denoting whether the trials should be randomized or not.
#' }
#' \item{
#' Trial strings use a specific syntax. Take the string "30AX(US)/10A", for example.
#' \itemize{
#' \item{
#' The "/" character is used to denote different trials. Above, the string specifies two trial types.
#' }
#' \item{
#' Parentheses are used to denote complex stimuli (or stimuli that need to be specifically named). Above, US is a named stimulus.
#' }
#' \item{
#' The numbers at the beginning of each trial type denote the number of trials given within a phase.
#' }}
#' }
#' }
#' @return A list with parsed results or a tibble with raw results
#' @seealso getHeidiOpts, parseDesign, trainPavHEIDI, parseHeidiResults
#' @export
runHeidi <- function(design_df, param_df = NULL, options = NULL, parse = T){
  default_alpha = .2
  parsed_design = parseDesign(design_df)
  auto_params = getParams(parsed_design, default_par = default_alpha) #generate parameters
  #check if parameters were passed
  if (is.null(param_df)){
    param_df = auto_params
    warning("No parameters supplied, using default values of %1.2f", default_alpha)
  }else{
    #check if the user-specified parameters match what the parser sees
    if (!(all(auto_params[, 1] %in% param_df[, 1]))){
      stop("Error: Found mismatch between user supplied stimulus names and those in the experimental design.")
    }
  }
  #check if options were passed
  auto_opts = getHeidiOpts()
  if (is.null(options)){
    options = auto_opts
  }else{
    #check if the user covered all the options requested
    if (any(!(names(auto_opts) %in% names(options)))){
      stop("Error: Did not supply some of the options required to fit the model. Please see getHeidiOpts")
    }
  }
  #make the tibble
  heidi_df = makeHeidiArgs(parsed_design, param_df, options)
  #run the model
  results = heidi_df %>% dplyr::rowwise() %>% dplyr::mutate(mod_data = list(train_pavHEIDI(stim_alphas, stim_cons, genSSWeights(stim_names), tps, trials, trialnames)))
  if (parse){
    results = parseHeidiResults(results)
  }
  return(results)
}
