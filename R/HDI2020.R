#' Train the HDI2020 model
#'
#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param parameters A list containing the model parameters,
#' as returned by parameter_info().
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @returns A list with raw results

HDI2020 <- function(v = NULL, # nolint: object_name_linter.
                    parameters,
                    experience,
                    mapping, ...) {
  # Assign parameters
  alphas <- parameters$alphas

  # data initialization
  ntrials <- length(experience$tp)
  if (is.null(v)) {
    v <- gen_ss_weights(mapping$unique_functional_stimuli)
  }
  vs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, rownames(v), rownames(v))
  )
  rs <- vs
  as <- array(NA,
    dim = c(ntrials, nrow(v)),
    dimnames = list(NULL, rownames(v))
  )
  combvs <- chainvs <- vector("list", ntrials)
  fsnames <- rownames(v) # get functional stimuli names
  test_stims <- fsnames

  for (t in 1:ntrials) {
    # get pre functional and nominal stimuli
    fprestims <- mapping$trial_pre_func[[experience$tp[t]]]
    nprestims <- mapping$trial_pre_nomi[[experience$tp[t]]]
    # get post nominal stimuli
    fpoststims <- mapping$trial_post_func[[experience$tp[t]]]
    npoststims <- mapping$trial_post_nomi[[experience$tp[t]]]

    # compute combV for all stimuli
    combV <- .combV(
      v = v, pre_func = fprestims,
      post_func = test_stims, db_trial = t
    )

    # compute chainV for all stimuli without a similarity rule
    chainV <- .chainV(
      v = v,
      pre_func = fprestims,
      post_func = test_stims,
      db_trial = t
    )

    # identify absent stimuli and calculate their "retrieved" salience
    ralphas <- .getalphas(
      v = v,
      alphas_nomi = alphas,
      pre_nomi = nprestims,
      pre_func = fprestims,
      fsnames = fsnames,
      nomi2func = mapping$nomi2func,
      db_trial = t
    )
    # Distribute R
    r <- .distR(ralphas, combV, chainV, t)

    # save data
    vs[t, , ] <- v
    as[t, ] <- ralphas
    rs[t, , ] <- r
    combvs[[t]] <- combV
    chainvs[[t]] <- chainV

    # learn if we need to
    if (!experience$is_test[t]) {
      # make one-hot vector of pre functional stimuli (for learning)
      oh_fstims <- .makeOH(c(fprestims, fpoststims), fsnames)

      # get saliencies for learning
      lalphas <- stats::setNames(rep(0, length(fsnames)), fsnames)
      lalphas[mapping$nomi2func[c(nprestims, npoststims)]] <-
        alphas[c(nprestims, npoststims)]

      # Learn
      e <- oh_fstims %*% v # expectation
      err <- oh_fstims * lalphas - e # error
      d <- oh_fstims * lalphas %*% err # delta
      diag(d) <- 0
      v <- v + d
    }
  }
  results <- list(
    vs = vs,
    rs = rs,
    as = as,
    acts = list(combvs = combvs, chainvs = chainvs)
  )
  results
}
