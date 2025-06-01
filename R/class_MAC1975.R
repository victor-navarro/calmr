#' @noRd
methods::setClass("MAC1975",
  representation(v = "matrix"),
  contains = "CalmrModel",
  prototype = methods::prototype(
    model_name = "MAC1975",
    outputs = c("associabilities", "associations", "responses"),
    parameters = list(),
    default_parameters = list(
      name = c(
        "alphas", "min_alphas", "max_alphas",
        "betas_on", "betas_off", "lambdas", "thetas", "gammas"
      ),
      default_value = c(0.4, 0.1, 1.0, 0.4, 0.4, 1, .2, 0.3)
    ),
    .internal_states = c("parameters", "v"),
    .is_timed = FALSE,
    .associations = "associations",
    .dnames_map = list(
      "responses" = c("s1", "s2"),
      "associations" = c("s1", "s2"),
      "associabilities" = c("s1")
    ),
    .parse_map = list(
      "responses" = .parse_nd,
      "associations" = .parse_nd,
      "associabilities" = .parse_2d
    ),
    .formula_map = list(
      "responses" = "s2",
      "associations" = "s2",
      "associabilities" = c()
    ),
    .plots_map = list(
      "responses" = plot_targetted_trials,
      "associations" = plot_targetted_trials,
      "associabilities" = plot_trials
    )
  )
)

#' @noRd
setMethod(
  "run", "MAC1975", function(object, experience, mapping, ...) {
    # assert the model has parameters
    .assert_has_parameters(object)
    parameters <- object@parameters

    # No functional stimuli check
    .assert_no_functional(mapping)

    # No functional stimuli check
    .assert_no_functional(mapping)

    # data initialization
    ntrials <- length(experience$tp)
    stim_names <- mapping$unique_nominal_stimuli

    v <- object@v
    if (!nrow(v)) {
      v <- .gen_ss_weights(stim_names)
    } else {
      v <- .expand_ss_weights(object@v, stim_names)
    }
    vs <- rs <- array(NA,
      dim = c(ntrials, dim(v)),
      dimnames = list(NULL, stim_names, stim_names)
    )
    as <- array(NA,
      dim = c(ntrials, nrow(v)),
      dimnames = list(NULL, stim_names)
    )

    # make matrix for attentional learning
    nsmat <- abs(diag(length(stim_names)) - 1)

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
        trial_periods <- length(mapping$period_nominals[[tn]])
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
          # gather onehot stimuli for both periods
          ps_ohs <- mapping$period_ohs[[tn]][[p]] |
            mapping$period_ohs[[tn]][[p2]]
          # generate period expectation
          pe <- v * p_ohs
          # calculate period error (with both periods)
          err <- ps_ohs * t(plambdas - t(pe))
          # calculate period delta
          d <- t(t(p_ohs * palphas * err) * pbetas)
          diag(d) <- 0
          # update weights
          v <- v + d

          # learn alphas for the stimuli in the current period
          # note: the expressions below take the expectation matrix,
          # pool it twice (once for each predictor, once for all i
          # but the predictor) and then sweeps each entry
          # with the lambda for each j.
          alphasd <- parameters$thetas * p_ohs *
            rowSums(
              abs(t((plambdas - t(pe) %*% nsmat) * parameters$gammas))
              - abs(t((plambdas - t(pe)) * parameters$gammas))
            )
          parameters$alphas <- parameters$alphas + alphasd
          # apply lower limit on parameters$alphas
          parameters$alphas[] <- sapply(
            seq_len(length(stim_names)), function(i) {
              max(parameters$min_alphas[i], parameters$alphas[i])
            }
          )
          # apply upper limit on parameters$alphas
          parameters$alphas[] <- sapply(
            seq_len(length(stim_names)), function(i) {
              min(parameters$max_alphas[i], parameters$alphas[i])
            }
          )
        }
      }
    }
    object@parameters <- parameters
    object@v <- v
    object@.last_experience <- experience
    object@.last_results <- list(
      associations = vs,
      associabilities = as,
      responses = rs
    )
    object
  }
)
