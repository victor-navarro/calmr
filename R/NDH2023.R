#' Train the NDH2023 model
#'
#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param parameters A list containing the model parameters,
#' as returned by `get_parameters()`.
#' @param timings A list of containing model timings, as returned
#' by `get_timings()`
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @param ... Additional named arguments
#' @return A list with raw results
#' @noRd

NDH2023 <- function(parameters, # nolint: object_name_linter.
                    experience,
                    timings,
                    mapping, ...) {
  total_trials <- length(unique(experience$trial))
  fsnames <- mapping$unique_functional_stimuli
  nstims <- length(fsnames)

  ws <- vector("list", length = total_trials)
  max_tsteps <- max(experience$b_to)
  step_labs <- seq_len(max_tsteps) * timings$time_resolution

  # array for onehots
  base_onehot <- array(0,
    dim = c(nstims, max_tsteps),
    dimnames = list(fsnames, step_labs)
  )
  # array for keeping track of stimulus components
  scomps <- array(0, dim = nstims, dimnames = list(fsnames))

  # eye to calculate error
  eye <- rbind(rep(0, max_tsteps), diag(max_tsteps))

  # preallocate a prediction matrix (s, s, t)
  pw <- array(0,
    dim = c(nstims, nstims, max_tsteps),
    dimnames = list(fsnames, fsnames, step_labs)
  )
  es <- drop(pw[1, , ])

  # Calculate all possible alphas based on trial types
  all_alphas <- .get_possible_alphas(experience, base_onehot, parameters)

  # Initialize the "smaller" w
  w <- list()
  for (stim in fsnames) {
    w[[stim]] <- stats::setNames(rep(list(array(0,
      dim = c(nstims, max_tsteps),
      dimnames = list(fsnames, step_labs)
    )), max_tsteps), step_labs)
  }
  dummyw <- w[[1]][[1]]

  for (tn in seq_len(total_trials)) {
    # get trial data
    tdat <- experience[experience$trial == tn, ]
    # get trial onehot matrix of active components
    omat <- .onehot_mat(base_onehot, tdat$stimulus, tdat$b_from, tdat$b_to)
    # get alphas for the trial
    traces <- .get_traces(omat, parameters)
    # calculate maximum trial steps
    tsteps <- max(tdat$b_to)
    # save association matrix
    ws[[tn]] <- w

    # reset stim component counter
    scomps[] <- 0
    for (ti in seq_len(tsteps)) {
      print(omat)
      # only do something if there is something going on
      if (any(omat[, ti] > 0)) {
        browser()
        # increase component count
        scomps <- scomps + omat[, ti]
        # get prediction matrix
        # very slow
        pw[] <- 0
        for (stim in fsnames) {
          if (scomps[stim] > 0) {
            pw[stim, , ] <- w[[stim]][[scomps[stim]]]
          } else {
            pw[stim, , ] <- dummyw
          }
        }
        # get predictions
        es[] <- apply(pw, 3, function(wi) t(wi) %*% omat[, ti])

        # compute errors
        ds <- traces[, ti] * eye[scomps + 1, ] - es

        # apply errors
        for (stim in fsnames) {
          if (scomps[stim] > 0) {
            nw <- w[[stim]][[as.character(traces[stim, ti])]] +
              traces[stim, ti] * ds
            nw[stim, ][] <- 0
            w[[stim]][[scomps[stim]]] <- nw
          }
        }
      }
    }
  }
  return(list(associations = ws))
}

#' Get stimulus traces
#' @param omat A one-hot matrix with stimuli states across time
#' @param parameters Parameters as returned by `get_parameters()`
#' @return A numeric array with omat's dimensions.
#' @noRd
.get_traces <- function(omat, parameters) {
  # Stimuli that are presented
  presented <- rownames(omat)[rowSums(omat) > 0]
  traces <- omat
  traces[] <- 0

  for (p in presented) {
    onset <- as.logical(omat[p, 1])
    traces[p, 1] <- parameters$max_alphas[p] * omat[p, 1]
    for (ti in seq_len(ncol(omat))[-1]) {
      if (omat[p, ti]) {
        if (!onset) {
          onset <- TRUE
          traces[p, ti] <- parameters$max_alphas[p]
        } else {
          traces[p, ti] <- traces[p, ti - 1] * (1 - parameters$decay_on[p])
        }
      } else {
        if (onset) {
          traces[p, ti] <- traces[p, ti - 1] * (1 - parameters$decay_off[p])
        }
      }
    }
  }
  traces
}

# Returns a list with all possible alphas per stimulus
.get_possible_alphas <- function(
    experience, base_onehot, parameters,
    tolerance = "%1.3f") {
  # maximum duration of each trial
  durs <- with(experience, tapply(b_to, trial, max))
  udat <- unique(experience[, c("trial", "tn", "stimulus", "b_from", "b_to")])
  udat$tn <- paste(udat$tn, durs[udat$trial])
  udat <- unique(udat[, c("tn", "stimulus", "b_from", "b_to")])
  all_alphas <- do.call(cbind, lapply((udat$tn), function(tt) {
    traces <- with(
      udat[udat$tn == tt, ],
      .get_traces(.onehot_mat(base_onehot, stimulus, b_from, b_to), parameters)
    )
  }))
  uni_alphas <- apply(all_alphas, 1, unique, simplify = FALSE)
  lapply(uni_alphas, function(x) x[x > 0])
}
