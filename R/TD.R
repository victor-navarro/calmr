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
#' @param debug Logical specifying whether to print debug information.
#' @param debug_t Whether to invoke a `browser` at
#' the end of a timestep equal to debug_t.
#' @param ... Additional named arguments
#' @return A list with raw results
#' @note This model is in a highly experimental state. Use with caution.
#' @noRd

TD <- function(
    parameters, timings, experience,
    mapping, debug = FALSE, debug_t = -1,
    debug_ti = -1, ...) {
  total_trials <- length(unique(experience$trial))
  fsnames <- mapping$unique_functional_stimuli

  # get maximum trial duration
  max_tsteps <- max(experience$b_to)

  # array for onehots
  base_onehot <- array(0,
    dim = c(length(fsnames), max_tsteps),
    dimnames = list(fsnames, (1:(max_tsteps)) * timings$time_resolution)
  )

  # array for elegibilities
  es <- array(0,
    dim = c(total_trials, length(fsnames), max_tsteps),
    dimnames = list(
      NULL,
      fsnames, (1:(max_tsteps)) * timings$time_resolution
    )
  )
  # array for associations (weights)
  ws <- array(0,
    dim = c(total_trials, length(fsnames), length(fsnames), max_tsteps),
    dimnames = list(
      NULL, fsnames, fsnames,
      (1:(max_tsteps)) * timings$time_resolution
    )
  )

  # array for values
  vs <- array(0,
    dim = c(total_trials, length(fsnames), max_tsteps),
    dimnames = list(
      NULL, fsnames,
      (1:(max_tsteps)) * timings$time_resolution
    )
  )

  # now the smaller arrays that will get modified over trials
  w <- dd <- ws[1, , , ] # associations and their deltas
  v <- d <- vs[1, , ] # values and pooled deltas
  e <- es[1, , ]

  for (tn in seq_len(total_trials)) {
    # get trial data
    tdat <- experience[experience$trial == tn, ]
    # get trial onehot matrix of active components
    omat <- .onehot_mat(base_onehot, tdat$stimulus, tdat$b_from, tdat$b_to)
    # save association matrix
    ws[tn, , , ] <- w

    for (ti in seq_len(max_tsteps)) {
      # calculate value expectations for this timestep
      v[, ti] <- t(w[, , ti]) %*% omat[, ti]
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
            e <- e * (parameters$sigma *
              parameters$gamma)^decay_steps
          }
          # delta only depends on current prediction
          d[, ti] <- (omat[, ti] * parameters$lambdas) +
            (parameters$gamma * v[, ti])
        } else {
          # delta depends on current and previous prediction
          d[, ti] <- (omat[, ti] * parameters$lambdas) +
            (parameters$gamma * v[, ti]) -
            v[, ti - 1]
          # decay elegibilities by 1 timestep
          e <- e * parameters$sigma *
            parameters$gamma
        }
        # compute update
        rates <- parameters$alphas * e
        dd[] <- sapply(seq_len(max_tsteps), function(i) {
          x <- rates[, i] %*% t(d[, ti, drop = FALSE])
          # zero-out self-associations
          diag(x) <- 0
          x
        })
        # apply update
        w <- w + dd
        # add maximal trace of what just happened
        e[, ti] <- omat[, ti]
        #
        if (ti == debug_ti) browser()
      }
    }
    vs[tn, , ] <- v
    es[tn, , ] <- e

    if (tn == debug_t) browser()
    if (debug) message(tn)
  }
  list(associations = ws, values = vs, elegibilities = es)
}
