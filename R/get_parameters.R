#' Get model parameters
#' @param design An `data.frame` containing the experimental design.
#' @param model A string specifying a model. One in [supported_models()].
#' @return A list with model parameters depending on model
#' @export
#' @examples
#' block <- get_design("blocking")
#' get_parameters(block, model = "SM2007")
get_parameters <- function(design, model = NULL) {
  model <- .calmr_assert("supported_model", model)
  parsed_design <- .calmr_assert("parsed_design", design)
  # Get parameter information
  par_info <- parameter_info(model)
  # Get stimulus names from design
  stimuli <- mapping(parsed_design)$unique_nominal_stimuli
  # Get trial names from design
  trialnames <- mapping(parsed_design)$trial_names
  # Get period name information from design
  transnames <- mapping(parsed_design)$transitions
  # Determine stimulus typing
  globalpars <- sapply(par_info$name, .is_global_parameter, model = model)
  trialpars <- sapply(par_info$name, .is_trial_parameter, model = model)
  transpars <- sapply(par_info$name, .is_trans_parameter, model = model)
  stimpars <- !globalpars & !trialpars & !transpars
  # filter information
  gpar_info <- lapply(par_info, function(x) x[globalpars])
  tpar_info <- lapply(par_info, function(x) x[trialpars])
  ttpar_info <- lapply(par_info, function(x) x[transpars])
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

  trial_pars <- list()
  if (any(trialpars)) {
    trial_pars <- do.call(
      .named_pars,
      c(tpar_info, list(trialnames))
    )
  }

  trans_pars <- list()
  if (any(transpars)) {
    for (i in seq_len(length(ttpar_info$name))) {
      trans_pars[[ttpar_info$name[i]]] <- lapply(
        transnames,
        function(trial) {
          stats::setNames(rep(
            ttpar_info$default_value[i],
            length(trial)
          ), trial)
        }
      )
    }
  }

  c(stim_pars, global_pars, trial_pars, trans_pars)
}

.named_pars <- function(name, default_value, stimuli) {
  pars <- list()
  n <- length(stimuli)
  for (i in seq_along(name)) {
    pars[[name[i]]] <- stats::setNames(rep(default_value[i], n), stimuli)
  }
  pars
}
