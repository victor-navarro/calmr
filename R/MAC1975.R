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
#' @return A list with raw results
#' @noRd
MAC1975 <- function(v = NULL, # nolint: object_name_linter.
                    parameters,
                    experience,
                    mapping, ...) {
  # No functional stimuli check
  .assert_no_functional(mapping)

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
    # get onehot stimuli for the trial
    oh_fstims <- mapping$trial_ohs[[tn]]

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
        palphas <- plambdas <-
          stats::setNames(rep(0, length(fsnames)), fsnames)
        # populate alphas
        palphas[mapping$nomi2func[pnominals]] <-
          parameters$alphas[pnominals]
        # populate betas
        pbetas <- parameters$betas_off
        pbetas[mapping$nomi2func[pnominals]] <-
          parameters$betas_on[pnominals]
        # populate lambdas
        plambdas[mapping$nomi2func[pnominals]] <-
          parameters$lambdas[pnominals]


        # get period onehot stimuli
        p_ohs <- mapping$period_ohs[[tn]][[p]]
        # gather onehot stimuli for both periods
        ps_ohs <- mapping$period_ohs[[tn]][[p]] | mapping$period_ohs[[tn]][[p2]]
        # generate period expectation
        pe <- v * p_ohs
        # calculate period error (with both periods)
        err <- ps_ohs * t(plambdas - t(pe))
        # calculate period delta
        d <- t(t(p_ohs * palphas * err) * pbetas)
        diag(d) <- 0
        # update weights
        v <- v + d

        # learn alphas
        # note: the expressions below take the expectation matrix,
        # pool it twice (once for each predictor, once for all i
        # but the predictor) and then sweeps each entry
        # with the lambda for each j.
        alphasd <- parameters$thetas * ps_ohs *
          rowSums(
            abs(t((plambdas - t(pe) %*% nsmat) * parameters$gammas))
            - abs(t((plambdas - t(pe)) * parameters$gammas))
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
  }
  results <- list(
    associations = vs,
    associabilities = as,
    responses = rs
  )
  results
}
