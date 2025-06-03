#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param o (optional) A named matrix of dimensions S,S,S
#' (the operator switches).
#' @param debug A logical specifying whether to print
#' information for the comparison process. Defaults to FALSE.
#' @param comparator_func A function to be used in the comparison
#' process. Either `.witnauer_comparator_func`or `.comparator_func`.
#' @param ... Additional named arguments
#' @return A list with raw results
#' @note
#' Correct usage of the v and o parameters requires the
#' matrices to adhere to a specific format.
#' For v, entry i,j represents the association from
#' stimulus i to stimulus j.
#' For o, entry i,k,j represents the operator switch
#' between stimulus i and comparator k with respect to stimulus j.
#' @noRd
methods::setClass("SM2007",
  representation(
    v = "matrix", o = "array"
  ),
  contains = "CalmrModel",
  prototype = methods::prototype(
    model_name = "SM2007",
    outputs = c(
      "activations", "associations",
      "relative_activations", "operator_switches"
    ),
    parameters = list(),
    default_parameters = list(
      name = c(
        "alphas", "lambdas", "omegas", "rhos",
        "gammas", "taus", "order"
      ),
      default_value = c(0.4, 1, 0.2, 1, 1, 0.2, 1)
    ),
    .internal_states = c("parameters", "v", "o"),
    .is_timed = FALSE,
    .associations = "associations",
    .dnames_map = list(
      "activations" = c("s1", "s2"),
      "relative_activations" = c("s1", "s2"),
      "associations" = c("s1", "s2"),
      "operator_switches" = c("s1", "comparison", "s2")
    ),
    .parse_map = list(
      "activations" = .parse_nd,
      "relative_activations" = .parse_nd,
      "associations" = .parse_nd,
      "operator_switches" = .parse_nd
    ),
    .formula_map = list(
      "activations" = "s2",
      "relative_activations" = "s2",
      "associations" = "s2",
      "operator_switches" = c("s2", "comparison")
    ),
    .plots_map = list(
      "activations" = plot_targeted_trials,
      "relative_activations" = plot_targeted_trials,
      "associations" = plot_targeted_trials,
      "operator_switches" = function(data) {
        plot_targeted_complex_trials(data, "comparison")
      }
    )
  )
)


#' @rdname CalmrModel-methods
#' @param debug A logical to print debugging messages.
#' @param comparator_func The function for the comparator process.
setMethod(
  "run", "SM2007", function(object, experience,
                            mapping, debug = FALSE,
                            comparator_func = .witnauer_comparator_proc, ...) {
    # assert the model has parameters
    .assert_has_parameters(object)
    parameters <- object@parameters

    # No functional stimuli check
    .assert_no_functional(mapping)

    # data initialization
    ntrials <- length(experience$tp) # max trials
    stim_names <- mapping$unique_nominal_stimuli

    # association weights
    v <- object@v
    if (!nrow(v)) {
      v <- .gen_ss_weights(stim_names)
    } else {
      v <- .expand_ss_weights(v, stim_names)
    }
    # operator switches
    o <- object@o
    if (!nrow(o)) {
      o <- .gen_os_values(stim_names)
    } else {
      o <- .expand_os_weights(o, stim_names)
    }
    do <- o # deltas for o
    stim_names <- rownames(v)

    vs <- array(NA,
      dim = c(ntrials, dim(v)),
      dimnames = list(NULL, stim_names, stim_names)
    )
    act <- relact <- array(NA,
      dim = dim(v),
      dimnames = list(stim_names, stim_names)
    )
    acts <- relacts <- array(NA,
      dim = c(ntrials, dim(v)),
      dimnames = list(NULL, stim_names, stim_names)
    )
    os <- array(NA,
      dim = c(ntrials, dim(o)),
      dimnames = list(NULL, stim_names, stim_names, stim_names)
    )

    for (t in 1:ntrials) {
      # get pointers
      tn <- experience$tn[t]

      # get nominal, and onehot stimuli
      nstims <- mapping$trial_nominals[[tn]]
      oh_fstims <- mapping$trial_ohs[[tn]]

      # generate activations
      act <- t(
        t(oh_fstims * v) + oh_fstims *
          parameters$rhos * parameters$alphas
      )

      # do comparisons/generate relative activations
      relact[] <- 0
      present <- nstims
      absent <- setdiff(stim_names, present)
      for (j in absent) {
        for (i in present) {
          if (debug) message("\nActivating ", j, " via ", i, "\n\n")
          relact[i, j] <- comparator_func(
            act = act, i = i, j = j,
            K = stim_names, o = o,
            gammas = parameters$gammas,
            order = parameters$order,
            debug = debug
          )
        }
      }

      # save data
      vs[t, , ] <- v
      acts[t, , ] <- act
      relacts[t, , ] <- relact
      os[t, , , ] <- o

      # learn if we need to
      if (!experience$is_test[t]) {
        # get parameters$alphas betas and parameters$lambdas for learning
        talphas <- tlambdas <-
          stats::setNames(rep(0, length(stim_names)), stim_names)

        # populating vector with nominal stimuli values
        # as functional stimuli values
        talphas[mapping$nomi2func[nstims]] <-
          parameters$alphas[nstims]

        # vector is initialized as if all stimuli are absent
        tlambdas[mapping$nomi2func[nstims]] <-
          parameters$lambdas[nstims]

        # Learn associations
        # calculate prediction error for present stimuli
        err <- oh_fstims * t(tlambdas - t(v * oh_fstims))
        # get strengthening deltas
        ds <- t(t(oh_fstims * talphas * err) * talphas) # first delta

        # get weakening deltas
        dw <- t(
          t(oh_fstims * v) * as.numeric(!oh_fstims) *
            -parameters$omegas
        ) * oh_fstims * talphas

        dv <- ds + dw
        diag(dv) <- 0
        # now calculate deltas for operator switch
        # could be better written
        do[] <- 0
        for (i in stim_names) {
          for (j in stim_names) {
            d <- 1 - o[i, , j]
            if (!v[i, j]) {
              d <- d * parameters$taus[j] *
                parameters$alphas[i] * v[i, ] * v[, j]
            }
            do[i, , j] <- d
          }
        }
        # Apply learning
        v <- v + dv
        o <- o + do
      }
    }
    object@parameters <- parameters
    object@v <- v
    object@o <- o
    object@.last_experience <- experience
    object@.last_raw_results <- list(
      associations = vs,
      activations = acts,
      relative_activations = relacts,
      operator_switches = os
    )
    object
  }
)
