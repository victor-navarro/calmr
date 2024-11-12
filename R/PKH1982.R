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
      # we maintain the directionality of learning
      # for a description of how this is implemented, see the RW1972 model code
      trial_periods <- length(mapping$period_functionals[[tn]])
      for (p in seq_len(trial_periods)) {
        p2 <- min(p + 1, trial_periods) # clamp
        # gather the nominals for the periods
        pnominals <- union(
          mapping$period_nominals[[tn]][[p]],
          mapping$period_nominals[[tn]][[p2]]
        )

        # set parameters for learning
        plambdas <-
          stats::setNames(rep(0, length(fsnames)), fsnames)
        # populate lambdas
        plambdas[mapping$nomi2func[pnominals]] <-
          parameters$lambdas[pnominals]


        # get period onehot stimuli
        p_ohs <- mapping$period_ohs[[tn]][[p]]
        # gather onehot stimuli for both periods
        ps_ohs <- mapping$period_ohs[[tn]][[p]] | mapping$period_ohs[[tn]][[p2]]

        # calculate period expectation
        pee <- p_ohs %*% ev
        pie <- p_ohs %*% iv
        pne <- pee - pie # net
        # association deltas
        # excitatory
        ed <- p_ohs * parameters$alphas %*% t(
          plambdas * parameters$betas_ex
        )
        # only learn when expectation expectation is lower than lambda
        ed <- t(t(ed) * as.numeric(
          (plambdas - pne) > 0
        ))
        # inhibitory ; hack to collapse ne into a numeric
        id <- p_ohs * parameters$alphas %*% t(
          (plambdas - as.numeric(pne)) * parameters$betas_in
        )

        # only learn when expectation is higher than lambda
        id[id > 0] <- 0
        id <- abs(id)
        diag(ed) <- diag(id) <- 0

        # learn
        ev <- ev + ed
        iv <- iv + id

        # alpha deltas
        alphasd <- p_ohs %*% abs(parameters$gammas * (plambdas - pne))
        diag(alphasd) <- 0

        # Need to be careful here, as there is no decay for absent stimuli
        talphasd <- (1 - parameters$thetas) *
          parameters$alphas + parameters$thetas * rowSums(alphasd)
        # update just what was seen
        parameters$alphas[pnominals] <- talphasd[pnominals]
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
  }
  results <- list(
    associations = list(EV = evs, IV = ivs),
    associabilities = as,
    responses = rs
  )
  results
}
