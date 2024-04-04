#' Train the HD2022 model
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
HD2022 <- function(v = NULL, # nolint: object_name_linter.
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
  as <- array(NA,
    dim = c(ntrials, nrow(v)),
    dimnames = list(NULL, fsnames)
  )
  combvs <- chainvs <- vector("list", ntrials)

  alphas_avg <- tapply(
    parameters$alphas,
    mapping$nomi2func, mean
  ) # average saliencies

  for (t in 1:ntrials) {
    # get pointers
    tn <- experience$tn[t]

    # get functional and nominal stimuli
    fstims <- mapping$trial_functionals[[tn]]
    nstims <- mapping$trial_nominals[[tn]]
    # get one-hot vector of pre functional stimuli (for learning)
    oh_fstims <- mapping$trial_ohs[[tn]]

    # compute combV for all stimuli
    combV <- .combV(
      v = v, pre_func = fstims,
      post_func = fsnames
    )

    # compute chainV for all stimuli with a similarity rule
    chainV <- .chainVSim(
      as_nomi = parameters$alphas,
      as_avg = alphas_avg,
      v = v,
      pre_nomi = nstims,
      pre_func = fstims,
      post_func = fsnames
    )

    # identify absent stimuli and calculate their "retrieved" salience
    ralphas <- .getalphas(
      v = v,
      alphas_nomi = parameters$alphas,
      pre_nomi = nstims,
      pre_func = fstims,
      fsnames = fsnames,
      nomi2func = mapping$nomi2func
    )

    # Distribute R
    r <- .distR(ralphas, combV, chainV)

    # save data
    vs[t, , ] <- v
    as[t, ] <- ralphas
    rs[t, , ] <- r
    combvs[[t]] <- combV
    chainvs[[t]] <- chainV

    # learn if we need to
    if (!experience$is_test[t]) {
      # get one-hot vector of pre functional stimuli (for learning)
      oh_fstims <- mapping$trial_ohs[[tn]]

      # get saliencies for learning (nominal only)
      lalphas <- stats::setNames(rep(0, length(fsnames)), fsnames)
      lalphas[mapping$nomi2func[nstims]] <- parameters$alphas[nstims]

      # Learn
      e <- oh_fstims %*% v # expectation
      err <- oh_fstims * lalphas - e # error
      d <- oh_fstims * lalphas %*% err # delta
      diag(d) <- 0
      v <- v + d
    }
  }

  results <- list(
    associations = vs,
    responses = rs,
    activations = as,
    pools = list(combvs = combvs, chainvs = chainvs)
  )
  results
}
