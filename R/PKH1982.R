#' Train the PKH1982 model
#'
#' @param ev (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param iv (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param parameters A list containing the model parameters,
#' as returned by get_parameters().
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @param ... Additional named arguments
#' @return A list with raw results
#' @noRd
PKH1982 <- function(
    # nolint: object_name_linter.
    ev = NULL,
    iv = NULL, parameters,
    experience, mapping, ...) {
  .assert_no_functional(mapping)

  # data initialization
  ntrials <- length(experience$tp)
  fsnames <- mapping$unique_functional_stimuli

  if (is.null(ev)) {
    ev <- gen_ss_weights(fsnames)
  }
  if (is.null(iv)) {
    iv <- gen_ss_weights(fsnames)
  }
  rs <- array(NA,
    dim = c(ntrials, dim(ev)),
    dimnames = list(NULL, fsnames, fsnames)
  )
  as <- array(NA,
    dim = c(ntrials, nrow(ev)),
    dimnames = list(NULL, fsnames)
  )
  evs <- ivs <- array(NA,
    dim = c(ntrials, dim(ev)),
    dimnames = list(NULL, fsnames, fsnames)
  )

  for (t in 1:ntrials) {
    # get pointers
    tn <- experience$tn[t]
    # get nominal, and onehot stimuli
    oh_fstims <- mapping$trial_ohs[[tn]]
    nstims <- mapping$trial_nominals[[tn]]

    # generate expectations
    # first expectation
    ee <- oh_fstims %*% ev
    ie <- oh_fstims %*% iv
    ne <- ee - ie # net

    # generate responses
    r <- (ev * oh_fstims) - (iv * oh_fstims)

    # save data
    evs[t, , ] <- ev
    ivs[t, , ] <- iv
    as[t, ] <- parameters$alphas
    rs[t, , ] <- r

    # learn if we need to
    if (!experience$is_test[t]) {
      # get parameters for learning
      tlambdas <- stats::setNames(rep(0, length(fsnames)), fsnames)
      tlambdas[nstims] <- parameters$lambdas[nstims]

      # association deltas
      # excitatory
      ed <- oh_fstims * parameters$alphas %*% t(tlambdas * parameters$betas_ex)
      # only learn when expectation expectation is lower than lambda
      ed <- t(t(ed) * as.numeric(
        (tlambdas - ne) > 0
      ))

      # inhibitory ; hack to collapse ne into a numeric
      id <- oh_fstims * parameters$alphas %*% t(
        (tlambdas - as.numeric(ne)) * parameters$betas_in
      )
      # only learn when expectation is higher than lambda
      id[id > 0] <- 0
      id <- abs(id)

      diag(ed) <- diag(id) <- 0

      # alpha deltas
      alphasd <- oh_fstims %*% abs(parameters$gammas * (tlambdas - ne))
      diag(alphasd) <- 0

      # learn
      ev <- ev + ed
      iv <- iv + id

      # Need to be careful here, as there is no decay for absent stimuli
      talphasd <- (1 - parameters$thetas) *
        parameters$alphas + parameters$thetas * rowSums(alphasd)
      parameters$alphas[nstims] <- talphasd[nstims]
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
    associations = list(EV = evs, IV = ivs),
    associabilities = as,
    responses = rs
  )
  results
}
