#' Return a model function
#'
#' @description Given a model string, returns a model function that can be used elsewhere in the package.
#' @param model_name A string specifying the model to be returned. One of `supported_models()`.

get_model <- function(model_name){
  #checks
  .calmr_check("supported_model", model_name)
  get(model_name)
}
