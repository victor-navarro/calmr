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
    mapping, debug = FALSE, debug_t = -1, ...) {
  total_trials <- length(unique(experience$trial))
  fsnames <- mapping$unique_functional_stimuli

  # get maximum trial duration
  max_tsteps <- max(experience$b_to)
  # array for onehots
  base_onehot <- array(0,
    dim = c(length(fsnames), max_tsteps + 1),
    dimnames = list(fsnames, (1:(max_tsteps + 1)) * timings$time_resolution)
  )
  # array for elegibilities
  es <- array(0,
    dim = c(total_trials, length(fsnames), max_tsteps + 1),
    dimnames = list(
      NULL,
      fsnames, (1:(max_tsteps + 1)) * timings$time_resolution
    )
  )
  # arrays for associations and expectations
  vs <- qs <- array(0,
    dim = c(total_trials, length(fsnames), length(fsnames), max_tsteps + 1),
    dimnames = list(
      NULL, fsnames, fsnames,
      (1:(max_tsteps + 1)) * timings$time_resolution
    )
  )
  v <- bigq <- vs[1, , , ]
  q <- d <- e <- drop(es[1, , ])

  for (tn in seq_len(total_trials)) {
    # get trial data
    tdat <- experience[experience$trial == tn, ]
    # get trial onehot matrix
    omat <- .onehot_mat(base_onehot, tdat$stimulus, tdat$b_from, tdat$b_to)

    vs[tn, , , ] <- v
    qs[tn, , , ] <- bigq
    es[tn, , ] <- e


    q[] <- d[] <- 0

    for (ti in seq_len(max_tsteps)) {
      # expectations
      q[, ti] <- omat[ti, ] %*% v[, , ti]
      bigq[, , ti] <- omat[ti, ] * v[, , ti]
      if (ti == 1) {
        # specifics for the first epoch
        if (tn > 1) {
          # calculate how many steps we had since last ITI
          iti_steps <- (min(tdat$rtime) -
            with(
              experience[experience$trial == (tn - 1), ],
              max(rtime)
            )) / timings$time_resolution
          # decay elegibility traces
          e[, ti] <- e[, max_tsteps] * parameters$sigma *
            parameters$gamma^iti_steps
        }
        # calculate simple delta
        d[, ti] <- omat[ti, ] * parameters$lambdas -
          (0 - parameters$gamma * q[, ti])
      } else {
        # deltas depend on the previous time step
        d[, ti] <- omat[ti, ] * parameters$lambdas -
          (q[, ti - 1] - parameters$gamma * q[, ti])
      }
      # compute update
      dd <- d[, ti, drop = FALSE] %*% t(
        parameters$alphas * e[, ti, drop = FALSE]
      )
      # zero-out self-associations
      diag(dd) <- 0
      # apply update
      v[, , ti + 1] <- v[, , ti] + dd
      # decay elegibilities for the next time step
      e[, ti + 1] <- parameters$gamma * parameters$sigma *
        e[, ti] + omat[ti, ]
    }
  }
  list(vs = vs, qs = qs, es = es)
}
