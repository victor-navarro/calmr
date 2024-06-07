#' Set reward parameters for ANCCR model
#' @param parameters A list of parameters, as returned by [get_parameters()]
#' @param rewards A character vector specifying the reward stimuli.
#' Default = `c("US")`
#' @return A list of parameters
#' @note The default behaviour of `get_parameters` for the ANCCR model is to
#' set every reward-related parameter to its non-zero default value.
#' This function will set those parameters to zero for non-reward stimuli
#' @export

set_reward_parameters <- function(parameters, rewards = c("US")) {
  reward_parameters <- c("reward_magnitude", "betas")
  for (p in reward_parameters) {
    nonrewards <- names(parameters[[p]])[!(names(parameters[[p]]) %in% rewards)]
    parameters[[p]][nonrewards] <- 0
  }
  parameters
}

# exp is an experience from .build_experiment
.anccrize_experience <- function(
    exper, design, parameters, timings,
    log_fn = .get_time_logs, ...) {
  # Uses the vanilla experience to create time logs
  lapply(exper, function(g) {
    log_fn(g,
      design = design,
      parameters = parameters,
      timings = timings, ...
    )
  })
}

# args is a list coming from make_experiment
# it has (at least) experience, mapping, and parameters
.get_time_logs <- function(
    experience, design, parameters, timings,
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
          reward_mag = parameters$reward_magnitude[period_funcs[[p]]],
          row.names = NULL
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
  # jitter if necessary
  if (parameters$jitter > 0) {
    eventlog$time <- unlist(sapply(unique(eventlog$time), function(t) {
      tlen <- sum(eventlog$time == t)
      if (tlen > 1) {
        return(t + stats::rnorm(tlen) * parameters$jitter)
      } else {
        return(t)
      }
    }, simplify = FALSE))
    eventlog <- eventlog[order(eventlog$time), ]
  }
  row.names(eventlog) <- NULL
  eventlog
}


.anccr_get_alpha <- function(denom, parameters, timestep) {
  if (!as.logical(parameters$use_exact_mean)) {
    if (as.logical(parameters$use_timed_alpha)) {
      alphat <- exp(
        -parameters$alpha_exponent * timestep *
          (parameters$alpha_init - parameters$alpha_min) +
          parameters$alpha_min
      )
    } else {
      alphat <- parameters$alpha
    }
  } else {
    alphat <- 1 / denom
  }
  alphat
}

# to generate subsampling times
.seq_gen <- function(t1, t2, res) {
  t1 <- ceiling(t1 / res) * res
  if (t1 > t2) {
    return(numeric(0))
  }
  seq(t1, t2, res)
}
