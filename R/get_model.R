#' Return a model function
#'
#' @description Given a model string, returns a model function that can be used elsewhere in the package.
#' @param model A string specifying the model to be returned. One of `supported_models()`.
#' @export

get_model <- function(model) {
  # Check model is supported
  .calmr_assert("supported_model", model)
  get(model)
}
