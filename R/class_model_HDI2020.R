#' @noRd
methods::setClass("HDI2020",
  representation(v = "matrix"),
  contains = "CalmrModel",
  prototype = methods::prototype(
    model_name = "HDI2020",
    outputs = c("activations", "associations", "pools", "responses"),
    parameters = list(),
    default_parameters = list(
      name = c("alphas"),
      default_value = c(0.4)
    ),
    .internal_states = c("parameters", "v"),
    .is_timed = FALSE,
    .associations = "associations",
    .dnames_map = list(
      "activations" = c("s1"),
      "pools" = c("s1", "s2"),
      "responses" = c("s1", "s2"),
      "associations" = c("s1", "s2")
    ),
    .parse_map = list(
      "activations" = .parse_2d,
      "pools" = .parse_typed_ragged,
      "responses" = .parse_nd,
      "associations" = .parse_nd
    ),
    .formula_map = list(
      "associations" = "s2",
      "activations" = c(),
      "pools" = c("s2", "type"),
      "responses" = "s2"
    ),
    .plots_map = list(
      "associations" = plot_targeted_trials,
      "pools" = plot_targeted_typed_trials,
      "responses" = plot_targeted_trials,
      "activations" = plot_trials
    )
  )
)



#' @rdname CalmrModel-methods
setMethod(
  "run", "HDI2020", function(object, experience, mapping, ...) {
    # assert the model has parameters
    .assert_has_parameters(object)
    parameters <- object@parameters

    # data initialization
    ntrials <- length(experience$tp)
    fsnames <- mapping$unique_functional_stimuli
    v <- object@v
    if (!nrow(v)) {
      v <- .gen_ss_weights(fsnames)
    } else {
      v <- .expand_ss_weights(v, fsnames)
    }
    fsnames <- rownames(v)
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
        v = v,
        pre_func = fstims,
        post_func = fsnames
      )

      # compute chainV for all stimuli without a similarity rule
      chainV <- .chainV(
        v = v,
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
        # get saliencies for learning
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
    object@parameters <- parameters
    object@v <- v
    object@.last_experience <- experience
    object@.last_raw_results <- list(
      associations = vs,
      responses = rs,
      activations = as,
      pools = list(combvs = combvs, chainvs = chainvs)
    )
    object
  }
)
