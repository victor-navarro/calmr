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

    # get nominal, and onehot stimuli
    nstims <- mapping$trial_nominals[[tn]]
    oh_fstims <- mapping$trial_ohs[[tn]]

    e <- oh_fstims %*% v # expectation
    r <- v * oh_fstims

    # save data
    vs[t, , ] <- v
    rs[t, , ] <- r

    # learn if we need to
    if (!experience$is_test[t]) {
      # get parameters$alphas betas and parameters$lambdas for learning
      talphas <- tbetas <- tlambdas <-
        stats::setNames(rep(0, length(fsnames)), fsnames)

      # populating vector with nominal stimuli values as
      # functional stimuli values
      talphas[mapping$nomi2func[nstims]] <-
        parameters$alphas[nstims]

      # vector is initialized as if all stimuli are absent
      tbetas <- betas_off_avg
      tbetas[mapping$nomi2func[nstims]] <-
        parameters$betas_on[nstims]

      tlambdas[mapping$nomi2func[nstims]] <-
        parameters$lambdas[nstims]

      err <- oh_fstims * talphas - e # error
      d <- oh_fstims * talphas %*% err # delta
      diag(d) <- 0
      v <- v + d
    }
  }
  results <- list(vs = vs, rs = rs)
  results
}
