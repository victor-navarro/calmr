#' Set reward parameters for ANCCR model
#' @param parameters A list of parameters, as returned by `get_parameters`
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


#' Augment CalmrDesign to be used with the ANCCR model.
#' @param object A CalmrDesign
#' @param reward_labels A character vector with reward names
#' @export
#' @rdname CalmrDesign-methods
.anccrize_design <- function(object, reward_labels = NULL) {
  if (is.null(reward_labels)) {
    reward_labels <- "US"
    warning("Using c('US') as reward_labels")
  }
  # Find which trial is rewarded using reward labels
  object@mapping$reward_labels <- reward_labels
  # Split into
  object@mapping$cue_labels <- setdiff(
    object@mapping$unique_functional_stimuli,
    object@mapping$reward_labels
  )
  object@mapping$is_reward <- lapply(
    object@mapping$trial_func, function(t) {
      t %in% object@mapping$reward_labels
    }
  )
  object@mapping$is_rewarded <- sapply(object@mapping$is_reward, any)
  object@mapping$has_cues <- lapply(
    object@mapping$trial_func, function(t) {
      any(t %in% object@mapping$cue_labels)
    }
  )
  object
}

# args are arguments from .build_experiment
.anccrize_arguments <- function(
    args, log_fn = calmr:::.get_time_logs) {
  # Uses the vanilla experience to create time logs
  args$experience <- apply(
    args, 1,
    log_fn,
    simplify = FALSE
  )
  args
}

# args is a tbl coming from make_experiment
# it has (at least) experience, mapping, and parameters
.get_time_logs <- function(
    args,
    debug = FALSE, ...) {
  experience <- args$experience
  mapping <- args$mapping
  pars <- args$parameters
  # Initialize eventlog
  eventlog <- data.frame()
  running_time <- 0

  for (ti in seq_len(nrow(experience))) {
    trial_name <- experience$tn[ti]
    transitions <- mapping$transitions[[trial_name]]

    # go through periods
    period_funcs <- mapping$period_functionals[[trial_name]]
    # sample start of the trial
    if (pars$use_exponential) {
      new_ts <- min(
        pars$max_ITI[trial_name],
        rexp(1, 1 / pars$mean_ITI[trial_name])
      )
    } else {
      new_ts <- runif(1) * pars$mean_ITI[trial_name] *
        0.4 + pars$mean_ITI[trial_name] * 0.8
    }
    running_time <- running_time + new_ts
    for (p in seq_len(length(period_funcs))) {
      eventlog <- rbind(
        eventlog,
        cbind(experience[ti, ],
          trial = ti,
          stimulus = period_funcs[[p]],
          time = running_time,
          reward_mag = pars$reward_magnitude[[period_funcs[[p]]]]
        )
      )
      # add delay if a transition is next
      if (p < length(period_funcs) && length(transitions)) {
        running_time <- running_time +
          pars$transition_delay[[trial_name]][[p]]
      }
    }
    # add post_trial delay
    running_time <- running_time +
      pars$post_trial_delay[[trial_name]]
  }
  row.names(eventlog) <- NULL
  eventlog
}
