#' Train the SM2007 model
#'
#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param o (optional) A named matrix of dimensions S,S,S
#' (the operator switches).
#' @param parameters A list containing the model parameters,
#' as returned by get_parameters().
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @param debug A logical specifying whether to print
#' information for the comparison process. Defaults to FALSE.
#' @param comparator_func A function to be used in the comparison
#' process. Either `.witnauer_comparator_func`or `.comparator_func`.
#' @param ... Additional named arguments
#' @return A list with raw results
#' @note
#' Correct usage of the v and o parameters requires the
#' matrices to adhere to a specific format.
#' For v, entry i,j represents the association from
#' stimulus i to stimulus j.
#' For o, entry i,k,j represents the operator switch
#' between stimulus i and comparator k with respect to stimulus j.
#' @noRd
SM2007 <- function(v = NULL, # nolint
                   o = NULL,
                   parameters,
                   experience,
                   mapping,
                   debug = FALSE,
                   comparator_func = .witnauer_comparator_proc,
                   ...) {
  # checks
  .assert_no_functional(mapping)

  # data initialization
  ntrials <- length(experience$tp) # max trials
  fsnames <- mapping$unique_functional_stimuli

  if (is.null(v)) {
    v <- gen_ss_weights(fsnames)
  } # association weights
  if (is.null(o)) {
    o <- gen_os_values(fsnames)
  } # operator switches
  do <- o # deltas for o
  fsnames <- rownames(v)

  vs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, fsnames, fsnames)
  )
  act <- relact <- array(NA,
    dim = dim(v),
    dimnames = list(fsnames, fsnames)
  )
  acts <- relacts <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, fsnames, fsnames)
  )
  os <- array(NA,
    dim = c(ntrials, dim(o)),
    dimnames = list(NULL, fsnames, fsnames, fsnames)
  )

  for (t in 1:ntrials) {
    # get pointers
    tn <- experience$tn[t]

    # get nominal, and onehot stimuli
    nstims <- mapping$trial_nominals[[tn]]
    oh_fstims <- mapping$trial_ohs[[tn]]

    # generate activations
    act <- t(
      t(oh_fstims * v) + oh_fstims *
        parameters$rhos * parameters$alphas
    )

    # do comparisons/generate relative activations
    relact[] <- 0
    present <- nstims
    absent <- setdiff(fsnames, present)
    for (j in absent) {
      for (i in present) {
        if (debug) message("\nActivating", j, "via", i, "\n\n")
        relact[i, j] <- comparator_func(
          act = act, i = i, j = j,
          K = fsnames, o = o,
          gammas = parameters$gammas,
          order = parameters$order,
          debug = debug
        )
      }
    }

    # save data
    vs[t, , ] <- v
    acts[t, , ] <- act
    relacts[t, , ] <- relact
    os[t, , , ] <- o

    # learn if we need to
    if (!experience$is_test[t]) {
      # get parameters$alphas betas and parameters$lambdas for learning
      talphas <- tlambdas <-
        stats::setNames(rep(0, length(fsnames)), fsnames)

      # populating vector with nominal stimuli values
      # as functional stimuli values
      talphas[mapping$nomi2func[nstims]] <-
        parameters$alphas[nstims]

      # vector is initialized as if all stimuli are absent
      tlambdas[mapping$nomi2func[nstims]] <-
        parameters$lambdas[nstims]

      # Learn associations
      # calculate prediction error for present stimuli
      err <- oh_fstims * t(tlambdas - t(v * oh_fstims))
      # get strengthening deltas
      ds <- t(t(oh_fstims * talphas * err) * talphas) # first delta

      # get weakening deltas
      dw <- t(
        t(oh_fstims * v) * as.numeric(!oh_fstims) *
          -parameters$omegas
      ) * oh_fstims * talphas

      dv <- ds + dw
      diag(dv) <- 0
      # now calculate deltas for operator switch
      # could be better written
      do[] <- 0
      for (i in fsnames) {
        for (j in fsnames) {
          d <- 1 - o[i, , j]
          if (!v[i, j]) {
            d <- d * parameters$taus[j] *
              parameters$alphas[i] * v[i, ] * v[, j]
          }
          do[i, , j] <- d
        }
      }
      # Apply learning
      v <- v + dv
      o <- o + do
    }
  }
  results <- list(
    associations = vs,
    activations = acts,
    relative_activations = relacts,
    operator_switches = os
  )
  results
}
