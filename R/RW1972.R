#' Train the RW1972 model
#'
#' @param v (optional) A named matrix of dimensions S,S;
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
RW1972 <- function(v = NULL, # nolint: object_name_linter.
                   parameters,
                   experience,
                   mapping, ...) {
  # data initialization
  ntrials <- length(experience$tp)
  fsnames <- mapping$unique_functional_stimuli

  if (is.null(v)) {
    v <- gen_ss_weights(fsnames)
  }
  vs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, fsnames, fsnames)
  )
  rs <- vs
  betas_off_avg <- tapply(
    parameters$betas_off,
    mapping$nomi2func, mean
  ) # average saliencies

  for (t in 1:ntrials) {
    # get pointers
    tn <- experience$tn[t]

    # get onehot stimuli
    oh_fstims <- mapping$trial_ohs[[tn]]
    # get response
    r <- v * oh_fstims

    # save data
    vs[t, , ] <- v # associations
    rs[t, , ] <- r # responses

    # learn if we need to
    if (!experience$is_test[t]) {
      # to maintain the directionality of learning
      # we need to look one period ahead
      # for situations in which there is only one period
      # (or if we are in the last period)
      # we just clamp the next period to the current period
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
        pbetas <- betas_off_avg
        pbetas[mapping$nomi2func[pnominals]] <-
          parameters$betas_on[pnominals]
        # populate lambdas
        plambdas[mapping$nomi2func[pnominals]] <-
          parameters$lambdas[pnominals]


        # get period onehot stimuli
        p_ohs <- mapping$period_ohs[[tn]][[p]]
        # generate period expectation
        pe <- p_ohs %*% v
        # calculate period error (with both periods)
        err <- plambdas - pe
        # calculate period delta
        d <- p_ohs * palphas %*% err
        diag(d) <- 0
        # update weights
        v <- v + d
      }
    }
  }
  results <- list(associations = vs, responses = rs)
  results
}
