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
  .assert_no_functional(mapping)

  # data initialization
  ntrials <- length(experience$tp)
  stim_names <- mapping$unique_nominal_stimuli

  if (is.null(v)) {
    v <- gen_ss_weights(stim_names)
  }
  vs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, stim_names, stim_names)
  )
  rs <- vs

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
          stats::setNames(rep(0, length(stim_names)), stim_names)
        # populate alphas
        palphas[pnominals] <- parameters$alphas[pnominals]
        # populate betas
        pbetas <- parameters$betas_off
        pbetas[pnominals] <- parameters$betas_on[pnominals]
        # populate lambdas
        plambdas[pnominals] <- parameters$lambdas[pnominals]

        # get period onehot stimuli
        p_ohs <- mapping$period_ohs[[tn]][[p]]
        # generate period expectation
        pe <- p_ohs %*% v
        # calculate period error (with both periods)
        err <- plambdas - pe
        # calculate period delta
        d <- t(t(p_ohs * palphas %*% err) * pbetas)
        diag(d) <- 0
        # update weights
        v <- v + d
      }
    }
  }
  results <- list(associations = vs, responses = rs)
  results
}
