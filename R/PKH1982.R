#' Train the PKH1982 model
#'
#' @param ev (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param iv (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param parameters A list containing the model parameters,
#' as returned by parameter_info().
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @returns A list with raw results

PKH1982 <- function(
    ev = NULL,
    iv = NULL, parameters,
    experience, mapping, ...) {
  # Assign parameters
  alphas <- parameters$alphas
  min_alphas <- parameters$min_alphas
  max_alphas <- parameters$max_alphas
  betas_ex <- parameters$betas_ex
  betas_in <- parameters$betas_in
  lambdas <- parameters$lambdas
  thetas <- parameters$thetas
  gammas <- parameters$gammas

  .calmr_assert("no_functional_stimuli", mapping)

  # data initialization
  ntrials <- length(experience$tp)
  snames <- names(alphas) # get functional stimuli names
  nstim <- length(snames)

  if (is.null(ev)) {
    ev <- gen_ss_weights(snames)
  }
  if (is.null(iv)) {
    iv <- gen_ss_weights(snames)
  }

  rs <- array(NA,
    dim = c(ntrials, dim(ev)),
    dimnames = list(NULL, rownames(ev), rownames(ev))
  )
  as <- array(NA,
    dim = c(ntrials, nrow(ev)),
    dimnames = list(NULL, rownames(ev))
  )
  evs <- ivs <- vector("list", ntrials)

  for (t in 1:ntrials) {
    # get pre functional stimuli
    prestims <- mapping$trial_pre_func[[experience$tp[t]]]

    # get post functional stimuli
    poststims <- mapping$trial_post_func[[experience$tp[t]]]

    # join
    allstims <- c(prestims, poststims)

    # make one-hot vector of functional stimuli
    oh_prestims <- .makeOH(prestims, snames)
    oh_poststims <- .makeOH(poststims, snames)

    # generate expectations
    # first expectation
    ee1 <- oh_prestims %*% ev
    ie1 <- oh_prestims %*% iv
    ne1 <- ee1 - ie1 # net

    # second expectation
    ee2 <- oh_poststims %*% ev
    ie2 <- oh_poststims %*% iv
    ne2 <- ee2 - ie2 # net

    # generate expectation matrices
    r <- (ev * oh_prestims) - (iv * oh_prestims)

    # save data
    evs[[t]] <- ev
    ivs[[t]] <- iv
    as[t, ] <- alphas
    rs[t, , ] <- r

    # learn if we need to
    if (!experience$is_test[t]) {
      # get parameters for learning
      pre_tlambdas <- post_tlambdas <-
        stats::setNames(rep(0, length(snames)), snames)

      pre_tlambdas[allstims] <- lambdas[allstims]
      post_tlambdas[poststims] <- lambdas[poststims]

      # association deltas
      # first delta
      # excitatory
      ed1 <- oh_prestims * alphas %*% t(pre_tlambdas * betas_ex)
      # only learn when expectation expectation is lower than lambda
      ed1 <- t(t(ed1) * as.numeric(
        (pre_tlambdas - ne1) > 0
      ))

      # inhibitory ; hack to collapse ne1 into a numeric
      id1 <- oh_prestims * alphas %*% t(
        (pre_tlambdas - as.numeric(ne1)) * betas_in
      )
      # only learn when expectation is higher than lambda
      id1[id1 > 0] <- 0
      id1 <- abs(id1)

      # second delta
      ed2 <- oh_poststims * alphas %*% t(post_tlambdas * betas_ex)
      # only learn when expectation expectation is lower than lambda
      ed2 <- t(t(ed2) * as.numeric(
        (post_tlambdas - ne2) > 0
      ))

      id2 <- oh_poststims * alphas %*% t(
        (post_tlambdas - as.numeric(ne2)) * betas_in
      )
      # only learn when expectation is higher than lambda
      id2[id2 > 0] <- 0
      id2 <- abs(id2)
      diag(ed1) <- diag(ed2) <- diag(id1) <- diag(id2) <- 0

      # alpha deltas
      alphasd1 <- oh_prestims %*% abs(gammas * (pre_tlambdas - ne1))
      alphasd2 <- oh_poststims %*% abs(gammas * (post_tlambdas - ne2))
      diag(alphasd1) <- diag(alphasd2) <- 0

      # learn
      ev <- ev + ed1 + ed2
      iv <- iv + id1 + id2

      # Need to be careful here, as there is no decay for absent stimuli
      talphasd <- (1 - thetas) * alphas + thetas *
        (rowSums(alphasd1) + rowSums(alphasd2))
      alphas[allstims] <- talphasd[allstims]

      # apply lower limit on alphas
      alphas[] <- sapply(1:nstim, function(i) max(min_alphas[i], alphas[i]))
      # apply upper limit on alphas
      alphas[] <- sapply(1:nstim, function(i) min(max_alphas[i], alphas[i]))
    }
  }
  results <- list(
    eivs = list(evs = evs, ivs = ivs),
    as = as,
    rs = rs
  )
  results
}
