#' Obtain model parameters
#' @param design An data.frame containing the experimental design.
#' @param model A string specifying a model. One in `supported_models()`.
#' @return A list with model parameters depending on model
#' @seealso \code{\link{supported_models}}
#' @export

get_parameters <- function(design, model = NULL) {
  model <- .calmr_assert("supported_model", model)
  parsed_design <- .calmr_assert("parsed_design", design)

  # Get stimulus names from design
  stimuli <- parsed_design@mapping$unique_nominal_stimuli
  do.call(.named_pars, c(parameter_info(model), list(stimuli)))
}

.named_pars <- function(name, default_value, stimuli) {
  pars <- list()
  n <- length(stimuli)
  for (i in seq_along(name)) {
    pars[[name[i]]] <- stats::setNames(rep(default_value[i], n), stimuli)
  }
  pars
}
