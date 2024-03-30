# all arguments for this come from .build_experiment
.tdrize_experience <- function(
    exper, design, parameters, timings,
    log_fn = .td_logs, ...) {
  # Uses the vanilla experience to create time logs
  lapply(exper, function(g) {
    log_fn(g,
      design = design,
      parameters = parameters,
      timings = timings, ...
    )
  })
}

.td_logs <- function(
    experience, design,
    parameters, timings,
    debug = FALSE, ...) {
  mapping <- design@mapping
  rownames(experience) <- NULL
  # Initialize eventlog
  eventlog <- data.frame()
  running_time <- 0
  for (ti in seq_len(nrow(experience))) {
    trial_name <- experience$tn[ti]
    transitions <- mapping$transitions[[trial_name]]
    period_funcs <- mapping$period_functionals[[trial_name]]
    # sample start of the trial
    if (timings$use_exponential) {
      new_ts <- min(
        with(timings$trial_ts, max_ITI[trial == trial_name]),
        .round_t(
          stats::rexp(
            1, 1 / with(timings$trial_ts, mean_ITI[trial == trial_name])
          ),
          timings$time_resolution
        )
      )
    } else {
      new_ts <- .round_t(
        stats::runif(1) *
          with(timings$trial_ts, mean_ITI[trial == trial_name]) *
          0.4 +
          with(
            timings$trial_ts,
            mean_ITI[trial == trial_name]
          ) * 0.8, timings$time_resolution
      )
    }
    running_time <- running_time + new_ts
    for (p in seq_along(period_funcs)) {
      eventlog <- rbind(
        eventlog,
        data.frame(experience[ti, ],
          stimulus = period_funcs[[p]],
          time = running_time,
          row.names = NULL
        )
      )
      # add (max) stimulus duration before delay
      running_time <- running_time +
        max(
          with(
            timings$period_ts,
            stimulus_duration[
              trial == trial_name &
                period == names(period_funcs)[p]
            ]
          )
        )

      # add delay if a transition is next
      if (p < length(period_funcs) && length(transitions)) {
        running_time <- running_time +
          with(
            timings$transition_ts,
            transition_delay[trial == trial_name][p]
          )
      }
    }
    # add post_trial delay
    running_time <- running_time +
      with(
        timings$trial_ts,
        post_trial_delay[trial == trial_name]
      )
  }
  row.names(eventlog) <- NULL
  eventlog
}

.round_t <- function(t, res) {
  round(t / res) * res
}
