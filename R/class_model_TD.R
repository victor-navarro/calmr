#' @noRd
methods::setClass("TD",
  representation(w = "list", e = "list"),
  contains = "CalmrModel",
  prototype = methods::prototype(
    model_name = "TD",
    outputs = c("values", "associations", "eligibilities"),
    parameters = list(),
    default_parameters = list(
      name = c(
        "alphas", "betas_on", "betas_off",
        "lambdas", "gamma", "sigma"
      ),
      default_value = c(
        0.05, 0.4, 0.4,
        1, 0.95, 0.90
      )
    ),
    .internal_states = c("parameters", "v"),
    .is_timed = FALSE,
    .associations = "associations",
    .dnames_map = list(
      "associations" = c("s1", "s2", "t_bin", "value"),
      "eligibilities" = c("s1", "t_bin", "value"),
      "values" = c("s1", "t_bin")
    ),
    .parse_map = list(
      "values" = .parse_nd,
      "eligibilities" = .parse_nested_ragged,
      "associations" = .parse_nested_ragged
    ),
    .formula_map = list(
      "associations" = c("s2", "t_bin"),
      "eligibilities" = "t_bin",
      "values" = "t_bin"
    ),
    .plots_map = list(
      "associations" = function(data, ...) {
        p <- plot_targeted_tbins(data, ...)
        # change x-label label
        t <- max(p$data$trial)
        p + ggplot2::labs(
          x = sprintf("Onset-relative Time Bin (Trial: %s)", t)
        )
      },
      "values" = plot_tbins,
      "eligibilities" = plot_tbins
    )
  )
)

#' @rdname CalmrModel-methods
setMethod(
  "run", "TD", function(object, experience, mapping, timings, ...) {
    # assert the model has parameters
    .assert_has_parameters(object)
    parameters <- object@parameters

    # No functional stimuli check
    .assert_no_functional(mapping)

    total_trials <- length(unique(experience$trial))
    stim_names <- mapping$unique_nominal_stimuli

    # join betas
    betas <- cbind(parameters$betas_off, parameters$betas_on)
    colnames(betas) <- c("off", "on")

    # get maximum trial duration
    max_tsteps <- max(experience$b_to)

    # array for onehots
    base_onehot <- array(0,
      dim = c(length(stim_names), max_tsteps),
      dimnames = list(stim_names, (1:(max_tsteps)) * timings$time_resolution)
    )

    # array for eligibilities
    es <- array(0,
      dim = c(total_trials, length(stim_names), max_tsteps),
      dimnames = list(
        NULL,
        stim_names, (1:(max_tsteps)) * timings$time_resolution
      )
    )
    ws <- es <- vector("list", length = total_trials)

    # array for values
    vs <- array(0,
      dim = c(total_trials, length(stim_names), max_tsteps),
      dimnames = list(
        NULL, stim_names,
        (1:(max_tsteps)) * timings$time_resolution
      )
    )

    # calculate stimulus durations
    s_steps <- sapply(stim_names, function(stim) {
      if (stim %in% experience$stimulus) {
        with(
          experience[experience$stimulus == stim, ],
          max(b_to) - min(b_from) + 1
        )
      }
    }, simplify = FALSE)


    # now the smaller arrays that will get modified over trials (csc-based)
    w <- object@w
    e <- object@e
    for (stim in stim_names) {
      if (is.null(s_steps[[stim]])) {
        # backup in case stim not in experience
        s_steps[[stim]] <- max(unlist(s_steps))
      }
      w[[stim]] <- array(0,
        dim = c(length(stim_names), s_steps[[stim]]),
        dimnames = list(
          stim_names,
          seq_len(s_steps[[stim]]) * timings$time_resolution
        )
      )
      e[[stim]] <- array(0,
        dim = c(s_steps[[stim]]),
        dimnames = list(seq_len(s_steps[[stim]]) * timings$time_resolution)
      )
    }

    v <- d <- vs[1, , ] # values and pooled deltas (time-based)

    for (tn in seq_len(total_trials)) {
      # get trial data
      tdat <- experience[experience$trial == tn, ]
      # get trial onehot matrix of active components
      omat <- .onehot_mat(base_onehot, tdat$stimulus, tdat$b_from, tdat$b_to)
      # save association matrix
      ws[[tn]] <- w

      for (ti in seq_len(max_tsteps)) {
        # calculate value expectations for this timestep
        # build weight matrix
        present <- stim_names[which(omat[, ti] > 0)]
        if (length(present)) {
          tw <- sapply(present, function(stim) {
            w[[stim]][, sum(omat[stim, 1:ti])]
          })
          v[, ti] <- tw %*% omat[present, ti, drop = FALSE]
        } else {
          v[, ti][] <- 0
        }
        if (!any(tdat$is_test)) {
          if (ti == 1) {
            # special treatment of traces and deltas for the first timestep
            if (tn > 1) {
              # decay steps since last time traces were decayed (previous trial)
              decay_steps <- (min(tdat$rtime) -
                with(
                  experience[experience$trial == (tn - 1), ],
                  max(rtime)
                )) / timings$time_resolution
              for (stim in stim_names) {
                e[[stim]] <- e[[stim]] *
                  (parameters$sigma * parameters$gamma)^decay_steps
              }
            }
            # delta only depends on current prediction
            d[, ti] <- (parameters$gamma * v[, ti])
          } else {
            # delta depends on current and previous prediction
            d[, ti] <- (parameters$gamma * v[, ti]) -
              v[, ti - 1]
          }
          # add events to error term
          d[, ti] <- d[, ti] + (omat[, ti] * parameters$lambdas)

          # rates of learning
          rates <- sapply(stim_names, function(stim) {
            e[[stim]] * parameters$alphas[stim]
          }, simplify = FALSE)
          # compute updates
          # trial betas
          tbetas <- sapply(stim_names, function(i) betas[i, omat[i, ti] + 1])
          dd <- sapply(stim_names, function(stim) {
            dhold <- (d[, ti, drop = FALSE] * tbetas) %*% rates[[stim]]
            # zero-out self-associations
            dhold[stim, ][] <- 0
            dhold
          }, simplify = FALSE)

          # apply updates and eligibility traces
          for (stim in stim_names) {
            w[[stim]][] <- w[[stim]] + dd[[stim]]
            # decay eligibilities by 1 timestep
            e[[stim]] <- e[[stim]] * parameters$sigma * parameters$gamma
            # Add event to
            e[[stim]][sum(omat[stim, 1:ti])][] <-
              e[[stim]][sum(omat[stim, 1:ti])][] +
              omat[, ti][stim]
          }
        }
      }

      # Update the last step separately with a "ghost" step
      # delta only depends on the last prediction
      gd <- -v[, ti, drop = FALSE]
      # decay elegibilities
      for (stim in stim_names) {
        e[[stim]] <- e[[stim]] * parameters$sigma * parameters$gamma
      }

      # rates of learning
      rates <- sapply(stim_names, function(stim) {
        e[[stim]] * parameters$alphas[stim]
      }, simplify = FALSE)
      # compute updates
      # trial betas
      tbetas[] <- betas[, 1] # all off
      dd <- sapply(stim_names, function(stim) {
        dhold <- (gd * tbetas) %*% rates[[stim]]
        # zero-out self-associations
        dhold[stim, ][] <- 0
        dhold
      }, simplify = FALSE)

      # apply updates
      for (stim in stim_names) {
        w[[stim]][] <- w[[stim]] + dd[[stim]]
      }
      vs[tn, , ] <- v
      es[[tn]] <- e
    }

    object@parameters <- parameters
    object@w <- w
    object@e <- e
    object@.last_experience <- experience
    object@.last_raw_results <- list(
      associations = ws, values = vs, eligibilities = es
    )
    object
  }
)
