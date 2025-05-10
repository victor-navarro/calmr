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
    # sample start of the trial if needed
    if (!timings$sample_timings) {
      # no sampling: use mean ITI
      new_ts <- with(timings$trial_ts, mean_ITI[trial == trial_name])
    } else {
      if (timings$use_exponential) {
        new_ts <- min(
          with(timings$trial_ts, max_ITI[trial == trial_name]),
          stats::rexp(
            1, 1 / with(timings$trial_ts, mean_ITI[trial == trial_name])
          )
        )
      } else {
        new_ts <- stats::runif(1) *
          with(timings$trial_ts, mean_ITI[trial == trial_name]) *
          0.4 +
          with(
            timings$trial_ts,
            mean_ITI[trial == trial_name]
          ) * 0.8
      }
    }
    running_time <- running_time + new_ts
    for (p in seq_along(period_funcs)) {
      eventlog <- rbind(
        eventlog,
        data.frame(experience[ti, ],
          stimulus = period_funcs[[p]],
          time = running_time,
          rtime = .round_t(running_time, timings$time_resolution),
          duration = with(
            timings$period_ts,
            stimulus_duration[
              trial == trial_name &
                stimulus %in% period_funcs[[p]] &
                period == names(period_funcs)[p]
            ]
          ),
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
  # calculate ITIs
  # starts of every trial
  starts <- with(eventlog, tapply(rtime, trial, min))
  # end of every trial
  ends <- with(eventlog, tapply(rtime + duration, trial, max))
  # calculate all but one ITI
  itis <- starts[-1] - ends[-length(ends)]
  # get last trial type
  itis <- c(itis, sum(timings$trial_ts[
    timings$trial_ts$trial == utils::tail(eventlog, 1)$tn,
    c("post_trial_delay", "mean_ITI")
  ]))
  eventlog$iti <- itis[eventlog$trial]

  # calculate bins
  eventlog$b_from <- unlist(with(eventlog, lapply(unique(trial), function(tr) {
    rts <- rtime[trial == tr]
    (rts - min(rts)) / timings$time_resolution + 1
  })))

  eventlog$b_to <- unlist(with(eventlog, lapply(unique(trial), function(tr) {
    rts <- rtime[trial == tr]
    durs <- duration[trial == tr]
    (rts - min(rts) + durs) / timings$time_resolution
  })))

  row.names(eventlog) <- NULL
  eventlog
}

.round_t <- function(t, res) {
  round(t / res) * res
}

# creates a one_hot matrix for stimuli on a trial
.onehot_mat <- function(b, stims, froms, tos) {
  xs <- b
  xs[] <- 0
  for (i in seq_along(stims)) {
    xs[stims[i], froms[i]:tos[i]] <- 1
  }
  xs
}
