#' Return a model function
#'
#' @description Given a model string, returns a model function that can be used elsewhere in the package.
#' @param model_name A string specifying the model to be returned. One of `supported_models()`.
#' @note The model_name parameter matches published models as follows:
#' \itemize{
#' \item{HDI2020: Honey, Dwyer, and Illiescu (2020). Psych Review.}
#' \item{HD2022: Honey and Dwyer (2022). Psych Review.}
#' }

get_model <- function(model_name){
  #checks
  .calmr_check("supported_model", model_name)
  get(model_name)
}
