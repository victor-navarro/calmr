#' Get timing design parameters
#' @param design A `data.frame` containing the experimental design.
#' @return A list of timing design parameters.
#' @export
#' @examples
#' block <- get_design("blocking")
#' get_timings(block)
get_timings <- function(design) {
  parsed_design <- .assert_parsed_design(design)
  # Get trial names from design
  trialnames <- mapping(parsed_design)$trial_names
  # Get transition names from design
  transitions <- mapping(parsed_design)$transitions
  # Get period functionals from design
  period_functionals <- mapping(parsed_design)$period_functionals

  def_trials <- .default_trial_timings()
  trial_pars <- data.frame(trial = trialnames)
  for (p in seq_along(def_trials$name)) {
    trial_pars[, def_trials$name[p]] <- def_trials$default_value[p]
  }

  def_periods <- .default_stimulus_timings()
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

  trans_pars <- list()
  if (length(transitions)) {
    def_trans <- .default_transition_timings()

    trans_pars <- data.frame(trial = rep(
      names(transitions), sapply(transitions, length)
    ), transition = unname(unlist(transitions)))

    for (p in seq_along(def_trans$name)) {
      trans_pars[, def_trans$name[p]] <- def_trans$default_value[p]
    }
  }
  # bundle into list
  c(
    .default_global_timings(),
    list(
      trial_ts = trial_pars,
      period_ts = period_pars,
      transition_ts = trans_pars
    )
  )
}

# Default timing parameter information
.default_trial_timings <- function() {
  list(name = c(
    "post_trial_delay",
    "mean_ITI", "max_ITI"
  ), default_value = c(1, 30, 90, 1))
}
.default_stimulus_timings <- function() {
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
    "time_resolution" = 0.5
  )
}
