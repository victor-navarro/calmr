methods::setClass("OJA",
  representation(v = "matrix"),
  contains = "CalmrModel",
  prototype = methods::prototype(
    model_name = "OJA",
    outputs = c("associations", "responses"),
    parameters = list(),
    default_parameters = list(
      name = c("etas", "epsilon"),
      default_value = c(0.7, 0.01),
      is_global = c(FALSE, TRUE)
    ),
    .internal_states = c("parameters", "v"),
    .is_timed = FALSE,
    .associations = "associations",
    .dnames_map = list(
      "responses" = c("s1", "s2"),
      "associations" = c("s1", "s2")
    ),
    .parse_map = list(
      "responses" = calmr:::.parse_nd,
      "associations" = calmr:::.parse_nd
    ),
    .formula_map = list(
      "responses" = "s2",
      "associations" = "s2"
    ),
    .plots_map = list(
      "responses" = calmr::plot_targeted_trials,
      "associations" = calmr::plot_targeted_trials
    )
  )
)

methods::setMethod(
  "run", "OJA", function(object, experience, mapping, ...) {
    # assert the model has parameters
    calmr:::.assert_has_parameters(object)
    parameters <- object@parameters

    # data initialization
    ntrials <- length(experience$tp)
    stim_names <- mapping$unique_nominal_stimuli
    v <- object@v
    if (!nrow(v)) {
      v <- calmr:::.gen_ss_weights(stim_names,
        default_val = parameters$epsilon
      )
    } else {
      v <- calmr:::.expand_ss_weights(v, stim_names,
        devault_val = parameters$epsilon
      )
    }
    diag(v) <- 0 # no self-associations
    stim_names <- rownames(v)

    vs <- array(NA,
      dim = c(ntrials, dim(v)),
      dimnames = list(NULL, stim_names, stim_names)
    )
    rs <- vs

    for (t in 1:ntrials) {
      # get trial name
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
        trial_periods <- length(mapping$period_nominals[[tn]])
        for (p1 in seq_len(trial_periods)) {
          p2 <- min(p1 + 1, trial_periods) # clamp
          # gather the nominals for the periods
          pnominals <- union(
            mapping$period_nominals[[tn]][[p1]],
            mapping$period_nominals[[tn]][[p2]]
          )
          # make period onehot stimuli (input)
          x <- stats::setNames(
            rep(0, length(stim_names)),
            stim_names
          )
          x[pnominals] <- 1
          xout <- x %*% t(x) # outer product on input
          # funnily enough, we don't need to calculate x %*% v
          # note: don't be misled by the bold vectors in Oja's description
          dv <- parameters$etas * (xout * v - t(v) * xout * v * v)
          v <- v + dv
          # force non-zero weights to prevent weight death
          v[v < parameters$epsilon] <- parameters$epsilon
          diag(v) <- 0 # no self-associations
        }
      }
    }
    object@parameters <- parameters
    object@v <- v
    object@.last_experience <- experience
    object@.last_raw_results <- list(associations = vs, responses = rs)
    object
  }
)
