#' Train the SM2007 model
#'
#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param o (optional) A named matrix of dimensions S,S,S
#' (the operator switches).
#' @param parameters A list containing the model parameters,
#' as returned by parameter_info().
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @param debug A logical specifying whether to print
#' information for the comparison process. Defaults to FALSE.
#' @param comparator_func A function to be used in the comparison
#' process. Either `.witnauer_comparator_func`or `.comparator_func`.
#' @returns A list with raw results
#' @note
#' Correct usage of the v and o parameters requires the
#' matrices to adhere to a specific format.
#' For v, entry i,j represents the association from
#' stimulus i to stimulus j.
#' For o, entry i,k,j represents the operator switch
#' between stimulus i and comparator k with respect to stimulus j.

SM2007 <- function(v = NULL, # nolint
                   o = NULL,
                   parameters,
                   experience,
                   mapping,
                   debug = FALSE,
                   comparator_func = .witnauer_comparator_proc,
                   ...) {
  # Assign parameters
  alphas <- parameters$alphas
  lambdas <- parameters$lambdas
  omegas <- parameters$omegas
  rhos <- parameters$rhos
  gammas <- parameters$gammas
  taus <- parameters$taus
  order <- parameters$order

  # checks
  .calmr_assert("no_functional_stimuli", mapping)

  # data initialization
  ntrials <- length(experience$tp) # max trials
  if (is.null(v)) {
    v <- gen_ss_weights(mapping$unique_functional_stimuli)
  } # association weights
  if (is.null(o)) {
    o <- gen_os_values(mapping$unique_functional_stimuli)
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
    # get pre functional and nominal stimuli
    fprestims <- mapping$trial_pre_func[[experience$tp[t]]]
    nprestims <- mapping$trial_pre_nomi[[experience$tp[t]]]
    # get post nominal stimuli
    fpoststims <- mapping$trial_post_func[[experience$tp[t]]]
    npoststims <- mapping$trial_post_nomi[[experience$tp[t]]]

    # make one-hot vector of functional stimuli (for learning)
    oh_fstims <- .makeOH(c(fprestims, fpoststims), fsnames)
    oh_fprestims <- .makeOH(fprestims, fsnames)
    oh_fpoststims <- .makeOH(fpoststims, fsnames)

    # generate activations
    act <- t(t(oh_fprestims * v) + oh_fprestims * rhos * alphas)

    # do comparisons/generate relative activations
    relact[] <- 0
    present <- fprestims
    absent <- setdiff(fsnames, present)
    for (j in absent) {
      for (i in present) {
        if (debug) cat("\nActivating", j, "via", i, "\n\n")
        relact[i, j] <- comparator_func(
          act = act, i = i, j = j,
          K = fsnames, o = o,
          gammas = gammas, order = order, debug = debug
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
      # get alphas betas and lambdas for learning
      talphas <- pre_tlambdas <- post_tlambdas <-
        stats::setNames(rep(0, length(fsnames)), fsnames)

      # populating vector with nominal stimuli values
      # as functional stimuli values
      talphas[mapping$nomi2func[c(nprestims, npoststims)]] <-
        alphas[c(nprestims, npoststims)]

      # vector is initialized as if all stimuli are absent
      pre_tlambdas[mapping$nomi2func[c(nprestims, npoststims)]] <-
        lambdas[c(nprestims, npoststims)]
      post_tlambdas[mapping$nomi2func[c(npoststims)]] <-
        lambdas[c(npoststims)]

      # Learn associations
      # calculate prediction error for present stimuli
      err1 <- oh_fstims * t(pre_tlambdas - t(v * oh_fprestims))
      err2 <- oh_fpoststims * t(post_tlambdas - t(v * oh_fpoststims))
      # get strengthening deltas
      ds1 <- t(t(oh_fprestims * talphas * err1) * talphas) # first delta
      ds2 <- t(t(oh_fpoststims * talphas * err2) * talphas) # second delta

      # get weakening deltas
      dw1 <- t(
        t(oh_fprestims * v) * as.numeric(!oh_fstims) * -omegas
      ) * oh_fprestims * talphas
      dw2 <- t(
        t(oh_fpoststims * v) * as.numeric(!oh_fpoststims) * -omegas
      ) * oh_fpoststims * talphas

      dv <- ds1 + ds2 + dw1 + dw2
      diag(dv) <- 0
      # now calculate deltas for operator switch
      do[] <- 0
      for (i in fsnames) {
        for (j in fsnames) {
          d <- 1 - o[i, , j]
          if (!v[i, j]) {
            d <- d * taus[j] * alphas[i] * v[i, ] * v[, j]
          }
          do[i, , j] <- d
        }
      }
      # Apply learning
      v <- v + dv
      o <- o + do
    }
  }
  results <- list(vs = vs, acts = acts, relacts = relacts, os = os)
  results
}
