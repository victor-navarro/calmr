#' @noRd
methods::setClass("RAND",
  representation(v = "matrix"),
  contains = "CalmrModel",
  prototype = methods::prototype(
    model_name = "RAND",
    outputs = c("associations", "responses"),
    parameters = list(),
    default_parameters = list(
      name = c("alphas"),
      default_value = c(0.4)
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
      "responses" = plot_targeted_trials,
      "associations" = plot_targeted_trials
    )
  )
)

#' @noRd
setMethod(
  "run", "RAND", function(object, experience, mapping, ...) {
    # assert the model has parameters
    .assert_has_parameters(object)
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
      v <- .expand_ss_weights(v, stim_names)
    }
    stim_names <- rownames(v)


    vs <- rs <- array(NA,
      dim = c(ntrials, dim(v)),
      dimnames = list(NULL, stim_names, stim_names)
    )
    for (t in 1:ntrials) {
      # get pointers
      tn <- experience$tn[t]

      # get nominal, and onehot stimuli
      oh_fstims <- mapping$trial_ohs[[tn]]

      # randomize weight matrix
      v[] <- matrix(stats::runif(length(v), min = -1, max = 1), dim(v))

      # generate response matrix
      r <- v * oh_fstims

      # save data
      vs[t, , ] <- v
      rs[t, , ] <- r
    }

    object@parameters <- parameters
    object@v <- v
    object@.last_experience <- experience
    object@.last_results <- list(associations = vs, responses = rs)
    object
  }
)
