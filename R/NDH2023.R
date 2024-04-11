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
    dim = c(nstims, nstims),
    dimnames = list(fsnames, fsnames)
  )
  es <- drop(pw[1, ])

  # Calculate all possible alphas based on trial types
  alphas_n <- .get_possible_alphas(experience, base_onehot, parameters)
  alphas_str <- lapply(alphas_n, function(vals) sprintf("%1.3f", vals))

  # Calculate similarities
  alpha_sims <- lapply(
    alphas_n,
    function(a) sapply(a, .alphaSim, j = a)
  )


  # Initialize the "smaller" w
  w <- list()
  for (stim in fsnames) {
    w[[stim]] <- stats::setNames(rep(list(array(0,
      dim = nstims,
      dimnames = list(fsnames)
    )), length(alphas_str[[stim]])), alphas_str[[stim]])
  }
  dummyw <- w[[1]][[1]]

  for (tn in seq_len(total_trials)) {
    # get trial data
    tdat <- experience[experience$trial == tn, ]
    is_test <- unique(tdat$is_test)
    # get trial onehot matrix of active components
    omat <- .onehot_mat(base_onehot, tdat$stimulus, tdat$b_from, tdat$b_to)
    # get alphas for the trial
    traces <- .get_traces(omat, parameters)
    # get trace pointers
    tracesi <- .find_traces(fsnames, traces, alphas_n)
    # calculate maximum trial steps
    tsteps <- max(tdat$b_to)
    # save association matrix
    ws[[tn]] <- w

    # reset stim component counter
    scomps[] <- 0
    for (ti in seq_len(tsteps)) {
      # only learn if there is something going on
      # and we're not on a test
      if (any(omat[, ti] > 0) && !is_test) {
        # increase component count
        scomps <- scomps + omat[, ti]
        # get prediction matrix
        # very slow
        pw[] <- 0
        for (stim in fsnames) {
          if (omat[stim, ti]) {
            pw[stim, ] <- w[[stim]][[tracesi[[stim, ti]]]]
          } else {
            pw[stim, ] <- dummyw
          }
        }
        # get predictions
        es[] <- t(pw) %*% omat[, ti]

        # compute errors
        ds <- traces[, ti] - es

        # apply errors
        for (stim in fsnames) {
          if (scomps[stim] > 0) {
            d <- traces[stim, ti] * ds
            d[stim] <- 0
            w[[stim]][[tracesi[[stim, ti]]]] <-
              w[[stim]][[tracesi[[stim, ti]]]] +
              d
          }
        }
      }

      # Now the "fun" part
      # calculate sim-based expectation (forward)
      browser()
      se <- sapply(fsnames, function(stim) {
        sapply(seq_len(tsteps), function(ti) {
          if (length(tracesi[[stim, ti]])) {
            sims <- alpha_sims[[stim]][tracesi[[stim, ti]], ]
            colSums(t(simplify2array(w[[stim]])) * sims)
          } else {
            dummyw
          }
        })
      }, simplify = FALSE)

      # calculate sim-based combv
      scombv <- lapply(seq_len(tsteps), function(ti) {
        absents <- fsnames[!omat[, ti]]
        presents <- fsnames[omat[, ti]]
        .get_scombv(absents, presents, w, se, ti)
      })

      # calculate sim-based chainv
      schainv <- lapply(seq_len(tsteps), function(ti) {
        absents <- fsnames[!omat[, ti]]
        presents <- fsnames[omat[, ti]]

        res <- array(0,
          dim = c(length(absents), length(presents)),
          dimnames = list(paste0(presents, collapse = ","), absents)
        )
        if (length(absents) && length(presents)) {
          forwards <- do.call(
            rbind,
            sapply(presents, function(pr) {
              se[[pr]][absents, ti]
            }, simplify = FALSE)
          )
          inters <-
            res[] <- sapply(absents, function(ab) {
              forwards[ab] * (1 + backwards[[ab]])
            })
        }
        res

        .get_schainv(absents, presents, w, se, ti)
      })
    }
  }



  for (tn in seq_len(total_trials)) {

  }

  return(list(associations = ws))
}

.get_scombv <- function(absents, presents, w, fw, i) {
  res <- array(0,
    dim = c(length(absents), length(presents)),
    dimnames = list(paste0(presents, collapse = ","), absents)
  )
  if (length(absents) && length(presents)) {
    # calculate pooled backwards
    # the backward is just a sum of all component associations; likely will have
    # to change it to be specific to a specific component
    backwards <- sapply(absents, function(ab) {
      colSums(do.call(rbind, (lapply(w[[ab]], "[", presents))))
    }, simplify = FALSE)
    forwards <- colSums(do.call(
      rbind,
      lapply(presents, function(pr) {
        fw[[pr]][absents, i]
      })
    ))
    res[] <- sapply(absents, function(ab) {
      forwards[ab] * (1 + backwards[[ab]])
    })
  }
  res
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
  lapply(uni_alphas, function(x) sort(x[x > 0]))
}

.find_traces <- function(stims, traces, alphas) {
  t(sapply(stims, function(stim) {
    sapply(
      traces[stim, ],
      function(a) which(a == alphas[[stim]])
    )
  }))
}
