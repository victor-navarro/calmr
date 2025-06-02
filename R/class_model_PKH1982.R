#' @noRd
methods::setClass("PKH1982",
  representation(ev = "matrix", iv = "matrix"),
  contains = "CalmrModel",
  prototype = methods::prototype(
    model_name = "PKH1982",
    outputs = c("associabilities", "associations", "responses"),
    parameters = list(),
    default_parameters = list(
      name = c(
        "alphas", "min_alphas", "max_alphas",
        "betas_ex", "betas_in", "lambdas", "thetas", "gammas"
      ),
      default_value = c(0.4, 0.1, 1.0, 0.4, 0.3, 1, 1, 0.3)
    ),
    .internal_states = c("parameters", "ev", "iv"),
    .is_timed = FALSE,
    .associations = "associations",
    .dnames_map = list(
      "responses" = c("s1", "s2"),
      "associabilities" = c("s1"),
      "associations" = c("s1", "s2")
    ),
    .parse_map = list(
      "responses" = .parse_nd,
      "associabilities" = .parse_2d,
      "associations" = .parse_typed
    ),
    .formula_map = list(
      "responses" = "s2",
      "associabilities" = c(),
      "associations" = c("s2", "type")
    ),
    .plots_map = list(
      "responses" = plot_targeted_trials,
      "associabilities" = plot_trials,
      "associations" = plot_targeted_typed_trials
    )
  )
)

#' @noRd
setMethod(
  "run", "PKH1982", function(object, experience, mapping, ...) {
    # assert the model has parameters
    .assert_has_parameters(object)
    parameters <- object@parameters

    # No functional stimuli check
    .assert_no_functional(mapping)

    # data initialization
    ntrials <- length(experience$tp)
    stim_names <- mapping$unique_nominal_stimuli

    ev <- object@ev
    iv <- object@iv
    if (!nrow(ev)) {
      ev <- .gen_ss_weights(stim_names)
    } else {
      ev <- .expand_ss_weights(object@ev, stim_names)
    }
    if (!nrow(iv)) {
      iv <- .gen_ss_weights(stim_names)
    } else {
      iv <- .expand_ss_weights(object@iv, stim_names)
    }
    stim_names <- rownames(ev)

    rs <- array(NA,
      dim = c(ntrials, dim(ev)),
      dimnames = list(NULL, stim_names, stim_names)
    )
    as <- array(NA,
      dim = c(ntrials, nrow(ev)),
      dimnames = list(NULL, stim_names)
    )
    evs <- ivs <- array(NA,
      dim = c(ntrials, dim(ev)),
      dimnames = list(NULL, stim_names, stim_names)
    )

    for (t in 1:ntrials) {
      # get pointers
      tn <- experience$tn[t]
      # get nominal, and onehot stimuli
      oh_fstims <- mapping$trial_ohs[[tn]]

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
            stats::setNames(rep(0, length(stim_names)), stim_names)
          # populate lambdas
          plambdas[mapping$nomi2func[pnominals]] <-
            parameters$lambdas[pnominals]

          # get period onehot stimuli
          p_ohs <- mapping$period_ohs[[tn]][[p]]

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
          parameters$alphas[] <- sapply(
            seq_len(length(stim_names)),
            function(i) {
              max(parameters$min_alphas[i], parameters$alphas[i])
            }
          )
          # apply upper limit on parameters$alphas
          parameters$alphas[] <- sapply(
            seq_len(length(stim_names)),
            function(i) {
              min(parameters$max_alphas[i], parameters$alphas[i])
            }
          )
        }
      }
    }
    object@parameters <- parameters
    object@ev <- ev
    object@iv <- iv
    object@.last_experience <- experience
    object@.last_results <- list(
      associations = list(EV = evs, IV = ivs),
      associabilities = as,
      responses = rs
    )
    object
  }
)
