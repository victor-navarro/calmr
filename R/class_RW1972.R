methods::setClass("RW1972",
  representation(v = "matrix"),
  contains = "CalmrModel",
  prototype = methods::prototype(
    model_name = "RW1972",
    outputs = c("associations", "responses"),
    parameters = list(),
    default_parameters = list(
      name = c("alphas", "betas_on", "betas_off", "lambdas"),
      default_value = c(0.4, 0.4, 0.4, 1)
    ),
    .internal_states = c("parameters", "v"),
    .is_timed = FALSE,
    .associations = "associations",
    .dnames_map = list(
      "responses" = c("s1", "s2"),
      "associations" = c("s1", "s2")
    ),
    .parse_map = list(
      "responses" = .parse_nd,
      "associations" = .parse_nd
    ),
    .formula_map = list(
      "responses" = "s2",
      "associations" = "s2"
    ),
    .plots_map = list(
      "responses" = plot_targetted_trials,
      "associations" = plot_targetted_trials
    ),
    .last_results = list()
  )
)

#' @noRd
setMethod(
  "run", "RW1972", function(object, experience, mapping, ...) {
    # assert the model has parameters
    if (is.null(object@parameters) || length(object@parameters) == 0) {
      stop("Model parameters are not set. Use `parameters<-` to set them.")
    }
    parameters <- object@parameters
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
    object@parameters <- parameters
    object@v <- v
    object@.last_experience <- experience
    object@.last_results <- list(associations = vs, responses = rs)
    object
  }
)
