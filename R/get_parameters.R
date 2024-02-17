#' Obtain model parameters
#' @param design An data.frame containing the experimental design.
#' @param model A string specifying a model. One in `supported_models()`.
#' @return A list with model parameters depending on model
#' @seealso \code{\link{supported_models}}
#' @export

get_parameters <- function(design, model = NULL) {
  model <- .calmr_assert("supported_model", model)
  parsed_design <- .calmr_assert("parsed_design", design)

  # Get parameter information
  par_info <- parameter_info(model)
  # Get stimulus names from design
  stimuli <- parsed_design@mapping$unique_nominal_stimuli
  # Get stimulus-specific and global parameters
  globals <- sapply(par_info$name, .is_global_parameter, model = model)
  spar_info <- lapply(par_info, function(x) x[!globals])
  gpar_info <- lapply(par_info, function(x) x[globals])

  stim_pars <- do.call(
    .named_pars,
    c(spar_info, list(stimuli))
  )

  global_pars <- list()
  for (i in seq_len(length(gpar_info$name))) {
    global_pars[gpar_info$name[i]] <- gpar_info$default_value[i]
  }
  return(c(stim_pars, global_pars))
}

.named_pars <- function(name, default_value, stimuli) {
  pars <- list()
  n <- length(stimuli)
  for (i in seq_along(name)) {
    pars[[name[i]]] <- stats::setNames(rep(default_value[i], n), stimuli)
  }
  pars
}
