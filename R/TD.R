#' Train the TD model
#'
#' @param parameters A list containing the model parameters,
#' as returned by `get_parameters()`.
#' @param timings A list containing the design timings,
#' as returned by `get_timings()`.
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @param ... Additional named arguments
#' @return A list with raw results
#' @note This model is in a highly experimental state. Use with caution.
#' @noRd

TD <- function(
    parameters, timings, experience,
    mapping, ...) {
  total_trials <- length(unique(experience$trial))
  fsnames <- mapping$unique_functional_stimuli

  # join betas
  betas <- cbind(parameters$betas_off, parameters$betas_on)
  colnames(betas) <- c("off", "on")

  # get maximum trial duration
  max_tsteps <- max(experience$b_to)

  # array for onehots
  base_onehot <- array(0,
    dim = c(length(fsnames), max_tsteps),
    dimnames = list(fsnames, (1:(max_tsteps)) * timings$time_resolution)
  )

  # array for eligibilities
  es <- array(0,
    dim = c(total_trials, length(fsnames), max_tsteps),
    dimnames = list(
      NULL,
      fsnames, (1:(max_tsteps)) * timings$time_resolution
    )
  )
  ws <- es <- vector("list", length = total_trials)

  # array for values
  vs <- array(0,
    dim = c(total_trials, length(fsnames), max_tsteps),
    dimnames = list(
      NULL, fsnames,
      (1:(max_tsteps)) * timings$time_resolution
    )
  )

  # calculate stimulus durations
  s_steps <- sapply(fsnames, function(stim) {
    if (stim %in% experience$stimulus) {
      with(
        experience[experience$stimulus == stim, ],
        max(b_to) - min(b_from) + 1
      )
    }
  }, simplify = FALSE)


  # now the smaller arrays that will get modified over trials (csc-based)
  w <- e <- list()
  for (stim in fsnames) {
    if (is.null(s_steps[[stim]])) {
      # backup in case stim not in experience
      s_steps[[stim]] <- max(unlist(s_steps))
    }
    w[[stim]] <- array(0,
      dim = c(length(fsnames), s_steps[[stim]]),
      dimnames = list(
        fsnames,
        seq_len(s_steps[[stim]]) * timings$time_resolution
      )
    )
    e[[stim]] <- array(0,
      dim = c(s_steps[[stim]]),
      dimnames = list(seq_len(s_steps[[stim]]) * timings$time_resolution)
    )
  }

  v <- d <- vs[1, , ] # values and pooled deltas (time-based)

  for (tn in seq_len(total_trials)) {
    # get trial data
    tdat <- experience[experience$trial == tn, ]
    # get trial onehot matrix of active components
    omat <- .onehot_mat(base_onehot, tdat$stimulus, tdat$b_from, tdat$b_to)
    # save association matrix
    ws[[tn]] <- w

    for (ti in seq_len(max_tsteps)) {
      # calculate value expectations for this timestep
      # build weight matrix
      present <- fsnames[which(omat[, ti] > 0)]
      if (length(present)) {
        tw <- sapply(present, function(stim) {
          w[[stim]][, sum(omat[stim, 1:ti])]
        })
        v[, ti] <- tw %*% omat[present, ti, drop = FALSE]
      } else {
        v[, ti][] <- 0
      }
      if (!any(tdat$is_test)) {
        if (ti == 1) {
          # special treatment of traces and deltas for the first timestep
          if (tn > 1) {
            # decay steps since last time traces were decayed (previous trial)
            decay_steps <- (min(tdat$rtime) -
              with(
                experience[experience$trial == (tn - 1), ],
                max(rtime)
              )) / timings$time_resolution
            for (stim in fsnames) {
              e[[stim]] <- e[[stim]] *
                (parameters$sigma * parameters$gamma)^decay_steps
            }
          }
          # delta only depends on current prediction
          d[, ti] <- (parameters$gamma * v[, ti])
        } else {
          # delta depends on current and previous prediction
          d[, ti] <- (parameters$gamma * v[, ti]) -
            v[, ti - 1]
        }
        # add events to error term
        d[, ti] <- d[, ti] + (omat[, ti] * parameters$lambdas)

        # rates of learning
        rates <- sapply(fsnames, function(stim) {
          e[[stim]] * parameters$alphas[stim]
        }, simplify = FALSE)
        # compute updates
        # trial betas
        tbetas <- sapply(fsnames, function(i) betas[i, omat[i, ti] + 1])
        dd <- sapply(fsnames, function(stim) {
          dhold <- (d[, ti, drop = FALSE] * tbetas) %*% rates[[stim]]
          # zero-out self-associations
          dhold[stim, ][] <- 0
          dhold
        }, simplify = FALSE)

        # apply updates and eligibility traces
        for (stim in fsnames) {
          w[[stim]][] <- w[[stim]] + dd[[stim]]
          # decay eligibilities by 1 timestep
          e[[stim]] <- e[[stim]] * parameters$sigma * parameters$gamma
          # Add event to
          e[[stim]][sum(omat[stim, 1:ti])][] <-
            e[[stim]][sum(omat[stim, 1:ti])][] +
            omat[, ti][stim]
        }
      }
    }

    # Update the last step separately with a "ghost" step
    # delta only depends on the last prediction
    gd <- -v[, ti, drop = FALSE]
    # decay elegibilities
    for (stim in fsnames) {
      e[[stim]] <- e[[stim]] * parameters$sigma * parameters$gamma
    }

    # rates of learning
    rates <- sapply(fsnames, function(stim) {
      e[[stim]] * parameters$alphas[stim]
    }, simplify = FALSE)
    # compute updates
    # trial betas
    tbetas[] <- betas[, 1] # all off
    dd <- sapply(fsnames, function(stim) {
      dhold <- (gd * tbetas) %*% rates[[stim]]
      # zero-out self-associations
      dhold[stim, ][] <- 0
      dhold
    }, simplify = FALSE)

    # apply updates
    for (stim in fsnames) {
      w[[stim]][] <- w[[stim]] + dd[[stim]]
    }
    vs[tn, , ] <- v
    es[[tn]] <- e
  }
  list(associations = ws, values = vs, eligibilities = es)
}
