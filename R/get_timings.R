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
  # Get require timings for the model
  mod_timings <- .model_timings(model)

  # global parameters
  global_pars <- .default_global_timings()[mod_timings$global]

  # trial parameters
  trial_pars <- NULL
  def_trials <- .default_trial_timings()
  if (any(def_trials$name %in% mod_timings$trials)) {
    trial_pars <- data.frame(trial = trialnames)
    for (p in which(def_trials$name %in% mod_timings$trials)) {
      trial_pars[, def_trials$name[p]] <- def_trials$default_value[p]
    }
  }
  def_periods <- .default_period_timings()
  period_pars <- NULL
  if (any(def_periods$name %in% mod_timings$periods)) {
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
    for (p in which(def_periods$name %in% mod_timings$periods)) {
      period_pars[, def_periods$name[p]] <- def_periods$default_value[p]
    }
  }

  trans_pars <- NULL
  def_trans <- .default_transition_timings()
  if (
    length(transitions) &&
      any(def_trans$name %in% mod_timings$transitions)
  ) {
    trans_pars <- data.frame(trial = rep(
      names(transitions), sapply(transitions, length)
    ), transition = unname(unlist(transitions)))

    for (p in which(def_trans$name %in% mod_timings$transitions)) {
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

# Default timing parameter information
.default_trial_timings <- function() {
  list(name = c(
    "post_trial_delay",
    "mean_ITI", "max_ITI"
  ), default_value = c(1, 30, 90))
}
.default_period_timings <- function() {
  list(
    name = c("stimulus_duration"),
    default_value = c(1)
  )
}
.default_transition_timings <- function() {
  list(name = c(
    "transition_delay"
  ), default_value = c(1))
}
.default_global_timings <- function() {
  list(
    "use_exponential" = TRUE,
    "time_resolution" = 0.5,
    "sample_timings" = TRUE
  )
}

.model_timings <- function(model) {
  timings_map <- list(
    "ANCCR" = list(
      "global" = c("use_exponential", "sample_timings"),
      "transitions" = c("transition_delay"),
      "periods" = c(),
      "trials" = c("post_trial_delay", "mean_ITI", "max_ITI")
    ),
    "TD" = list(
      "global" = c("use_exponential", "time_resolution", "sample_timings"),
      "transitions" = c("transition_delay"),
      "periods" = c("stimulus_duration"),
      "trials" = c("post_trial_delay", "mean_ITI", "max_ITI")
    )
  )
  if (is.null(mode)) {
    return(timings_map)
  }
  timings_map[[model]]
}
