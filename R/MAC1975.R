#' Train the MAC1975 model
#'
#' @param parameters A list containing the model parameters,
#' as returned by get_parameters().
#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @param ... Additional named arguments
#' @returns A list with raw results

MAC1975 <- function(v = NULL, # nolint: object_name_linter.
                    parameters,
                    experience,
                    mapping, ...) {
  # No functional stimuli check
  .calmr_assert("no_functional_stimuli", mapping)

  # data initialization
  ntrials <- length(experience$tp)
  fsnames <- mapping$unique_functional_stimuli

  if (is.null(v)) {
    v <- gen_ss_weights(fsnames)
  }

  vs <- rs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, fsnames, fsnames)
  )
  as <- array(NA,
    dim = c(ntrials, nrow(v)),
    dimnames = list(NULL, fsnames)
  )

  # make matrix for attentional learning
  nsmat <- abs(diag(length(fsnames)) - 1)

  for (t in 1:ntrials) {
    # get pointers
    tn <- experience$tn[t]
    # get nominal, and onehot stimuli
    oh_fstims <- mapping$trial_ohs[[tn]]
    nstims <- mapping$trial_nominals[[tn]]

    # generate expectation and response matrices
    # note the local error
    e <- v * oh_fstims
    r <- e

    # save data
    vs[t, , ] <- v
    as[t, ] <- parameters$alphas
    rs[t, , ] <- r

    # learn if we need to
    if (!experience$is_test[t]) {
      talphas <- tbetas <- tlambdas <-
        stats::setNames(rep(0, length(fsnames)), fsnames)
      # populating vector with nominal stimuli
      # values as functional stimuli values
      talphas[nstims] <- parameters$alphas[nstims]

      # betas and lambdas vectors
      # are initialized as if all stimuli are absent
      tbetas <- parameters$betas_off
      tbetas[nstims] <- parameters$betas_on[nstims]
      tlambdas[nstims] <- parameters$lambdas[nstims]

      # Learn associations
      err <- oh_fstims * t(tlambdas - t(e))
      d <- t(t(oh_fstims * talphas * err) * tbetas)

      diag(d) <- 0
      v <- v + d

      # Learn alphas
      # note: the expressions below take the expectation matrix,
      # pools it twice (once for each predictor, once for all i
      # but the predictor) and sweeps each with the lambda for each j.
      alphasd <- parameters$thetas * oh_fstims *
        rowSums(
          abs(t((tlambdas - t(r) %*% nsmat) * parameters$gammas))
          - abs(t((tlambdas - t(r)) * parameters$gammas))
        )
      parameters$alphas <- parameters$alphas + alphasd
      # apply lower limit on parameters$alphas
      parameters$alphas[] <- sapply(seq_len(length(fsnames)), function(i) {
        max(parameters$min_alphas[i], parameters$alphas[i])
      })
      # apply upper limit on parameters$alphas
      parameters$alphas[] <- sapply(seq_len(length(fsnames)), function(i) {
        min(parameters$max_alphas[i], parameters$alphas[i])
      })
    }
  }
  results <- list(
    vs = vs,
    as = as,
    rs = rs
  )
  results
}
