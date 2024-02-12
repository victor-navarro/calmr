#' Train the SM2007 model
#'
#' @param alphas A named vector with stimulus saliences.
#' @param lambdas A named vector with learning asymptotes.
#' @param omegas A named vector with weakening rates (extinction).
#' @param rhos A named vector with weights for activation of present stimuli.
#' @param gammas A named vector with weights for contribution of each stimulus in comparison processes.
#' @param taus A named vector with stimulus-specific rates for operator switch.
#' @param orders An integer specifying the order of the comparison process. Defaults to 1.
#' @param V (optional) A named matrix of dimensions S,S (the associative strengths); where S is the number of stimuli.
#' @param O (optional) A named matrix of dimensions S,S,S (the operator switches).
#' @param experience A data.frame specifying trials as rows, as returned by `make_model_args`
#' @param mapping A named list specifying trial and stimulus mapping, as returned by `make_model_args`
#' @param debug A logical specifying whether to print information for the comparison process. Defaults to FALSE.
#' @param comparator_func A function to be used in the comparison process. Either `.witnauer_comparator_func`or `.comparator_func`.
#' @return A list with
#' \itemize{
#' \item{vs, Array of dimensions P,S,S; where P is the number of trials used to train the model and S is the number of stimuli involved in the experiment. Contains stimulus association strengths.}
#' \item{acts} As above but with stimulus activations.
#' \item{relacts} As above but with (relative) stimulus activations after comparison process.
#' \item{os} Array of dimensions P,S,S,S, with the state of the operator switches. See notes related to `O` parameter.
#' \item{other, Carryover arguments used for further processing by other functions}
#' }
#' @note
#' Correct usage of the V and O parameters requires the matrices to adhere to a specific format.
#' For V, entry i,j represents the association from stimulus i to stimulus j.
#' For O, entry i,k,j represents the operator switch between stimulus i and comparator k with respect to stimulus j.

SM2007 <- function(alphas,
                   lambdas,
                   omegas,
                   rhos,
                   gammas,
                   taus,
                   orders,
                   V = NULL,
                   O = NULL,
                   experience,
                   mapping,
                   debug = F,
                   comparator_func = .witnauer_comparator_proc,
                   ...) {
  mod <- methods::new("CalmrModel",
    model = "SM2007"
  )

  # checks
  .calmr_assert("comparator_order", orders)
  .calmr_assert("no_functional_stimuli", mapping)

  # data initialization
  ntrials <- length(experience$tp) # max trials
  if (is.null(V)) {
    V <- gen_ss_weights(mapping$unique_functional_stimuli)
  } # association weights
  if (is.null(O)) {
    O <- gen_os_values(mapping$unique_functional_stimuli)
  } # operator switches
  dO <- O # deltas for O
  order <- unique(orders)
  fsnames <- rownames(V)

  vs <- array(NA,
    dim = c(ntrials, dim(V)),
    dimnames = list(NULL, fsnames, fsnames)
  )
  act <- relact <- array(NA,
    dim = dim(V),
    dimnames = list(fsnames, fsnames)
  )
  acts <- relacts <- array(NA,
    dim = c(ntrials, dim(V)),
    dimnames = list(NULL, fsnames, fsnames)
  )
  os <- array(NA,
    dim = c(ntrials, dim(O)),
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
    act <- t(t(oh_fprestims * V) + oh_fprestims * rhos * alphas)

    # do comparisons/generate relative activations
    relact[] <- 0
    present <- fprestims
    absent <- setdiff(fsnames, present)
    for (j in absent) {
      for (i in present) {
        if (debug) cat("\nActivating", j, "via", i, "\n\n")
        relact[i, j] <- comparator_func(
          act = act, i = i, j = j,
          K = fsnames, O = O,
          gammas = gammas, order = order, debug = debug
        )
      }
    }

    # save data
    vs[t, , ] <- V
    acts[t, , ] <- act
    relacts[t, , ] <- relact
    os[t, , , ] <- O

    # learn if we need to
    if (!experience$is_test[t]) {
      # get alphas betas and lambdas for learning
      talphas <- pre_tlambdas <- post_tlambdas <- stats::setNames(rep(0, length(fsnames)), fsnames)

      # populating vector with nominal stimuli values as functional stimuli values
      talphas[mapping$nomi2func[c(nprestims, npoststims)]] <- alphas[c(nprestims, npoststims)]

      # vector is initialized as if all stimuli are absent
      pre_tlambdas[mapping$nomi2func[c(nprestims, npoststims)]] <- lambdas[c(nprestims, npoststims)]
      post_tlambdas[mapping$nomi2func[c(npoststims)]] <- lambdas[c(npoststims)]

      # Learn associations
      # calculate prediction error for present stimuli
      err1 <- oh_fstims * t(pre_tlambdas - t(V * oh_fprestims))
      err2 <- oh_fpoststims * t(post_tlambdas - t(V * oh_fpoststims))
      # get strengthening deltas
      ds1 <- t(t(oh_fprestims * talphas * err1) * talphas) # first delta
      ds2 <- t(t(oh_fpoststims * talphas * err2) * talphas) # second delta

      # get weakening deltas
      dw1 <- t(t(oh_fprestims * V) * as.numeric(!oh_fstims) * -omegas) * oh_fprestims * talphas
      dw2 <- t(t(oh_fpoststims * V) * as.numeric(!oh_fpoststims) * -omegas) * oh_fpoststims * talphas

      dV <- ds1 + ds2 + dw1 + dw2
      diag(dV) <- 0

      # now calculate deltas for operator switch
      dO[] <- 0
      for (i in fsnames) {
        for (j in fsnames) {
          d <- 1 - O[i, , j]
          if (!V[i, j]) {
            d <- d * taus[j] * alphas[i] * V[i, ] * V[, j]
          }
          dO[i, , j] <- d
        }
      }

      # Apply learning
      V <- V + dV
      O <- O + dO
    }
  }
  mod@parameters <- list(
    alphas = alphas,
    lambdas = lambdas,
    omegas = omegas,
    rhos = rhos,
    gammas = gammas,
    taus = taus,
    orders = orders
  )
  mod@model_results <- list(vs = vs, acts = acts, relacts = relacts, os = os)
  mod@experience <- experience
  mod@mapping <- mapping
  mod
}
