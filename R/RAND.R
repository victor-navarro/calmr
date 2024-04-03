#' Train the RAND model
#'
#' @param v (optional) A named matrix of dimensions S,S;
#' where S is the number of stimuli.
#' @param parameters A list containing the model parameters,
#' as returned by get_parameters().
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @param ... Additional named arguments
#' @return A list with raw results
#' @noRd
RAND <- function(v = NULL, # nolint: object_name_linter.
                 parameters,
                 experience,
                 mapping, ...) {
  # data initialization
  ntrials <- length(experience$tp)
  fsnames <- mapping$unique_functional_stimuli

  if (is.null(v)) {
    v <- gen_ss_weights(fsnames)
  }

  vs <- rs <- array(NA,
    dim = c(ntrials, dim(v)),
    dimnames = list(NULL, fsnames, fsnames)
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
  results <- list(associations = vs, responses = rs)
  results
}
