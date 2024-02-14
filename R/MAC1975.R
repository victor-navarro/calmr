#' Train the MAC1975 model
#'
#' @param parameters A list containing the model parameters,
#' as returned by parameter_info().
#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @returns A list with raw results

MAC1975 <- function(v = NULL, # nolint: object_name_linter.
                    parameters,
                    experience,
                    mapping, ...) {
  # Assign parameters
  alphas <- parameters$alphas
  min_alphas <- parameters$min_alphas
  max_alphas <- parameters$max_alphas
  betas_on <- parameters$betas_on
  betas_off <- parameters$betas_off
  lambdas <- parameters$lambdas
  thetas <- parameters$thetas
  gammas <- parameters$gammas


  # No functional stimuli check
  .calmr_assert("no_functional_stimuli", mapping)

  # data initialization
  ntrials <- length(experience$tp)
  snames <- names(alphas) # get functional stimuli names
  nstim <- length(snames)

  if (is.null(v)) {
    v <- gen_ss_weights(snames)
  }

  vs <- rs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, rownames(v), rownames(v))
  )
  as <- array(NA,
    dim = c(ntrials, nrow(v)),
    dimnames = list(NULL, rownames(v))
  )

  # make matrix for attentional learning
  nsmat <- abs(diag(nstim) - 1)

  for (t in 1:ntrials) {
    # get pre functional stimuli
    prestims <- mapping$trial_pre_func[[experience$tp[t]]]

    # get post functional stimuli
    poststims <- mapping$trial_post_func[[experience$tp[t]]]

    # join
    allstims <- c(prestims, poststims)

    # make one-hot vector of functional stimuli
    oh_stims <- .makeOH(allstims, snames)
    oh_prestims <- .makeOH(prestims, snames)
    oh_poststims <- .makeOH(poststims, snames)

    # generate expectation matrices
    r <- v * oh_prestims
    post_emat <- v * oh_poststims

    # learn if we need to
    if (!experience$is_test[t]) {
      # get alphas betas and lambdas for learning
      talphas <- tbetas <- pre_tlambdas <-
        post_tlambdas <- stats::setNames(rep(0, length(snames)), snames)
      # populating vector with nominal stimuli
      # values as functional stimuli values
      talphas[allstims] <- alphas[allstims]

      # betas and lambdas vectors are initialized as if all stimuli are absent
      tbetas <- betas_off
      tbetas[allstims] <- betas_on[allstims]

      pre_tlambdas[allstims] <- lambdas[allstims]
      post_tlambdas[poststims] <- lambdas[poststims]

      # Learn associations
      # first error (includes all stimuli in the sequence)
      err1 <- oh_stims * t(pre_tlambdas - t(r))
      # second error (includes only the second half stimuli)
      err2 <- oh_poststims * t(post_tlambdas - t(post_emat))

      d1 <- t(t(oh_prestims * talphas * err1) * tbetas) # first delta
      d2 <- t(t(oh_poststims * talphas * err2) * tbetas) # second delta

      diag(d1) <- diag(d2) <- 0
      v <- v + d1 + d2

      # Learn alphas
      # note: the expressions below take the expectation matrix,
      # pools it twice (once for each predictor, once for all i
      # but the predictor) and sweeps each with the lambda for each j.
      # we do it twice just like the associations above,
      # to take care of the associations in a sequential design
      alphasd1 <- thetas * oh_prestims *
        rowSums(
          abs(t((pre_tlambdas - t(r) %*% nsmat) * gammas))
          - abs(t((pre_tlambdas - t(r)) * gammas))
        )

      alphasd2 <- thetas * oh_poststims *
        rowSums(
          abs(t((post_tlambdas - t(post_emat) %*% nsmat) * gammas))
          - abs(t((post_tlambdas - t(post_emat)) * gammas))
        )

      alphas <- alphas + alphasd1 + alphasd2
      # apply lower limit on alphas
      alphas[] <- sapply(1:nstim, function(i) max(min_alphas[i], alphas[i]))
      # apply upper limit on alphas
      alphas[] <- sapply(1:nstim, function(i) min(max_alphas[i], alphas[i]))
    }

    # save data
    vs[t, , ] <- v
    as[t, ] <- alphas
    rs[t, , ] <- r
  }
  results <- list(
    vs = vs,
    as = as,
    rs = rs
  )
  results
}
