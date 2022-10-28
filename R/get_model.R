#' Return a model function
#'
#' @description Given a model string, returns a model function that can be used elsewhere in the package.
#' @param model_name A string specifying the model to be returned. One of `c("HDI2020", "HD2022", "RW1972)`.
#' @note
#' The model_name parameter matches published models as follows:
#' \itemize {
#' \item {HDI2020: Honey, Dwyer, and Illiescu (2020). Psych Review.}
#' \item {HD2022: Honey and Dwyer (2022). Psych Review.}
#' }
#' @export

get_model <- function(model_name){
  #checks
  supported_models = get_supported_models()
  if (is.null(model_name) | !(model_name %in% supported_models)){
    stop(paste("You must specificy model_name as one of:", paste(supported_models, collapse = ", ")))
  }

  model = switch(model_name,
                 "RW1972" = RW1972,
                 "HDI2020" = HDI2020,
                 "HD2022" = HD2022)
  model

}
