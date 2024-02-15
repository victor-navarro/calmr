#' Train the RW1972 model
#'
#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param parameters A list containing the model parameters,
#' as returned by parameter_info().
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @returns A CalmrModel class and some results <-

RW1972 <- function(v = NULL, # nolint: object_name_linter.
                   parameters,
                   experience,
                   mapping, ...) {
  # Assign parameters
  alphas <- parameters$alphas
  betas_on <- parameters$betas_on
  betas_off <- parameters$betas_off
  lambdas <- parameters$lambdas

  # data initialization
  ntrials <- length(experience$tp)
  if (is.null(v)) {
    v <- gen_ss_weights(mapping$unique_functional_stimuli)
  }

  vs <- rs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, rownames(v), rownames(v))
  )

  betas_off_avg <- tapply(
    betas_off,
    mapping$nomi2func, mean
  ) # average saliencies

  fsnames <- rownames(v) # get functional stimuli names

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

    # generate expectations
    e1 <- oh_fprestims %*% v # first expectation
    e2 <- oh_fpoststims %*% v # second expectation

    # generate full expectation matrix (only for data saving purposes)
    r <- v * oh_fprestims

    # learn if we need to
    if (!experience$is_test[t]) {
      # get alphas betas and lambdas for learning
      talphas <- tbetas <- pre_tlambdas <- post_tlambdas <-
        stats::setNames(rep(0, length(fsnames)), fsnames)

      # populating vector with nominal stimuli values as
      # functional stimuli values
      talphas[mapping$nomi2func[c(nprestims, npoststims)]] <-
        alphas[c(nprestims, npoststims)]

      # vector is initialized as if all stimuli are absent
      tbetas <- betas_off_avg
      tbetas[mapping$nomi2func[c(nprestims, npoststims)]] <-
        betas_on[c(nprestims, npoststims)]

      pre_tlambdas[mapping$nomi2func[c(nprestims, npoststims)]] <-
        lambdas[c(nprestims, npoststims)]
      post_tlambdas[mapping$nomi2func[c(npoststims)]] <-
        lambdas[c(npoststims)]


      # Learn
      # first error (includes all stimuli in the sequence)
      err1 <- oh_fstims * pre_tlambdas - e1
      # second error (includes only the second half stimuli)
      err2 <- oh_fpoststims * post_tlambdas - e2

      # first delta
      d1 <- t(t((oh_fprestims * talphas) %*% err1) * tbetas)
      # second delta
      d2 <- t(t((oh_fpoststims * talphas) %*% err2) * tbetas)

      diag(d1) <- diag(d2) <- 0

      v <- v + d1 + d2 # learn
    }

    # save data
    vs[t, , ] <- v
    rs[t, , ] <- r
  }
  results <- list(vs = vs, rs = rs)
  results
}
