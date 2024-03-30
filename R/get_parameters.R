#' Get model parameters
#' @param design A `data.frame` containing the experimental design.
#' @param model A string specifying a model. One in [supported_models()].
#' @return A list with model parameters depending on model
#' @export
#' @examples
#' block <- get_design("blocking")
#' get_parameters(block, model = "SM2007")
get_parameters <- function(design, model = NULL) {
  # Assert the model is supported in the package
  model <- .assert_model(model)
  # Assert design
  parsed_design <- .assert_parsed_design(design)
  # Get parameter information
  par_info <- model_parameters(model)
  # Get stimulus names from design
  stimuli <- mapping(parsed_design)$unique_nominal_stimuli
  # Determine stimulus typing
  globalpars <- sapply(par_info$name, .is_global_parameter, model = model)
  stimpars <- !globalpars
  # filter information
  gpar_info <- lapply(par_info, function(x) x[globalpars])
  spar_info <- lapply(par_info, function(x) x[stimpars])

  stim_pars <- list()
  if (any(stimpars)) {
    stim_pars <- do.call(
      .named_pars,
      c(spar_info, list(stimuli))
    )
  }

  global_pars <- list()
  if (any(globalpars)) {
    for (i in seq_len(length(gpar_info$name))) {
      global_pars[gpar_info$name[i]] <- gpar_info$default_value[i]
    }
  }

  c(stim_pars, global_pars)
}

.named_pars <- function(name, default_value, stimuli) {
  pars <- list()
  n <- length(stimuli)
  for (i in seq_along(name)) {
    pars[[name[i]]] <- stats::setNames(rep(default_value[i], n), stimuli)
  }
  pars
}
