#' Get timing design parameters
#' @param design A `data.frame` containing the experimental design.
#' @param model One of [supported_timed_models()].
#' @return A list of timing design parameters.
#' @export
#' @examples
#' block <- get_design("blocking")
#' get_timings(block, model = "TD")
get_timings <- function(design, model) {
  parsed_design <- .assert_parsed_design(design)
  model <- .assert_timed_model(model)
  # Get trial names from design
  trialnames <- mapping(parsed_design)$trial_names
  # Get transition names from design
  transitions <- mapping(parsed_design)$transitions
  # Get period functionals from design
  period_functionals <- mapping(parsed_design)$period_functionals
  # Get required timings for the model
  mod_timings <- .model_timings(model)
  # global parameters
  global_pars <- NULL
  if (length(mod_timings$global$name)) {
    global_pars <- setNames(
      mod_timings$global$default_value,
      mod_timings$global$name
    )
  }
  # trial parameters
  trial_pars <- NULL
  def_trials <- mod_timings$trials
  if (length(def_trials$name)) {
    trial_pars <- data.frame(trial = trialnames)
    for (p in seq_along(def_trials$name)) {
      trial_pars[, def_trials$name[p]] <- def_trials$default_value[p]
    }
  }

  period_pars <- NULL
  def_periods <- mod_timings$periods
  if (length(def_periods$name)) {
    period_pars <- data.frame()
    for (t in trialnames) {
      for (p in names(period_functionals[[t]])) {
        period_pars <- rbind(
          period_pars,
          data.frame(
            trial = t, period = p,
            stimulus = period_functionals[[t]][[p]]
          )
        )
      }
    }
    for (p in seq_along(def_periods$name)) {
      period_pars[, def_periods$name[p]] <- def_periods$default_value[p]
    }
  }

  trans_pars <- NULL
  def_trans <- mod_timings$transitions
  # only add transitions if they exist in design
  if (
    length(transitions) &&
      length(def_trans$name)
  ) {
    trans_pars <- data.frame(trial = rep(
      names(transitions), sapply(transitions, length)
    ), transition = unname(unlist(transitions)))

    for (p in seq_along(def_trans$name)) {
      trans_pars[, def_trans$name[p]] <- def_trans$default_value[p]
    }
  }
  # bundle into list
  all_pars <- c(
    global_pars,
    list(
      trial_ts = trial_pars,
      period_ts = period_pars,
      transition_ts = trans_pars
    )
  )
  all_pars[!sapply(all_pars, is.null)]
}

.model_timings <- function(model) {
  methods::new(model)@timing_parameters
}
