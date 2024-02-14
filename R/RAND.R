#' Train the RAND model
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
RAND <- function(v = NULL, # nolint: object_name_linter.
                 parameters,
                 experience,
                 mapping, ...) {
  # data initialization
  ntrials <- length(experience$tp)
  if (is.null(v)) {
    v <- gen_ss_weights(mapping$unique_functional_stimuli)
  }

  vs <- rs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, rownames(v), rownames(v))
  )

  fsnames <- rownames(v) # get functional stimuli names

  for (t in 1:ntrials) {
    # get pre functional and nominal stimuli
    fprestims <- mapping$trial_pre_func[[experience$tp[t]]]
    # get post nominal stimuli
    fpoststims <- mapping$trial_post_func[[experience$tp[t]]]

    # make one-hot vector of functional stimuli (for learning)
    oh_fstims <- .makeOH(c(fprestims, fpoststims), fsnames)

    # randomize weight matrix
    v[] <- matrix(stats::runif(length(v), min = -1, max = 1), dim(v))

    # generate expectation matrix (only for data saving purposes)
    r <- apply(v, 2, function(x) x * oh_fstims)

    # save data
    vs[t, , ] <- v
    rs[t, , ] <- r
  }
  results <- list(vs = vs, rs = rs)
  results
}
