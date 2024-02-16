# x is a tbl (aggregated_results from CalmrExperiment@results)
# comparisons is a model-named list with model outputs
.rsa <- function(x, comparisons, .test = FALSE, ...) {
  # Assert the comparisons list is named
  modnames <- names(comparisons)
  if (!length(modnames)) {
    stop("Comparisons list must be a model-named list.")
  }
  # Assert all models in comparisons are in x
  if (!all(sapply(modnames, function(m) m %in% x$model))) {
    stop("Models listed in comparisons must be in x")
  }
  # Assert all model outputs are in x
  for (m in modnames) {
    for (o in comparisons[[m]]) {
      if (is.null(x[x$model == m, ][[o]][[1]])) {
        stop(sprintf("Output %s not found for model %s", o, m))
      }
    }
  }

  # Get trial matrices
  trial_mats <- sapply(modnames, function(m) {
    sapply(comparisons[[m]], function(o) {
      dat <- x[x$model == m, ][[o]][[1]]
      tsize <- unique(with(dat, tapply(value, trial, length)))
      if (length(tsize) > 1) {
        stop(sprintf("Output %s for model %s is inconsistent across trials.
        Cannot calculate distances"))
      }
      matrix(dat$value, ncol = tsize, byrow = TRUE)
    }, simplify = FALSE)
  }, simplify = FALSE)

  # calculate trial distance
  trial_dists <- lapply(trial_mats, function(m) {
    lapply(m, stats::dist, ...)
  })

  # unnest trial distances and put modelname in names
  flatnames <- sapply(
    modnames,
    function(m) sprintf("%s.%s", m, comparisons[[m]])
  )
  trial_dists <- stats::setNames(
    unlist(trial_dists, recursive = FALSE), flatnames
  )
  # calculate correlation matrix
  corr_mat <- stats::cor(data.frame(lapply(trial_dists, as.numeric)))

  # create CalmrRSA object
  obj <- methods::new("CalmrRSA",
    corr_mat = corr_mat,
    distances = trial_dists,
    args = list(comparisons = comparisons, ...)
  )

  if (.test) {
    obj <- .rsa_test(obj)
  }
  obj
}

# implements a permutation test for CalmrRSA object
# object is a CalmrRSA object
# n_samples is a integer
# p is a float
.rsa_test <- function(object, n_samples = 1e3, p = 0.95) {
  dist_dat <- data.frame(lapply(object@distances,
    as.numeric,
    check.names = FALSE
  ))

  # sample the distance data.frame and pass on the arguments for cor
  # could have written a wrapper
  cor_args <- object@args[intersect(
    names(object@args),
    methods::formalArgs(stats::cor)
  )]
  perm_cors <- replicate(
    n_samples,
    do.call(
      stats::cor,
      c(
        x = list(
          apply(dist_dat, 2, sample)
        ),
        cor_args
      )
    )
  )

  # get critical values
  lc <- apply(
    perm_cors, c(1, 2),
    function(x) stats::quantile(x, probs = (1 - p) / 2)
  )
  uc <- apply(
    perm_cors, c(1, 2),
    function(x) stats::quantile(x, probs = p + ((1 - p) / 2))
  )

  # get significance
  sig_mat <- object@corr_mat < lc | object@corr_mat > uc
  object@test_data <- list(
    sig_mat = sig_mat, lower_crit = lc,
    upper_crit = uc, n_samples = n_samples, p = p
  )
  object
}
