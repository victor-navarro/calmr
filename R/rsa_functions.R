#' Perform representational similarity analysis
#'
#' @param x A list of [CalmrExperiment-class] objects
#' @param comparisons A model-named list containing the model
#' outputs to compare.
#' @param test Whether to test the RSA via permutation test. Default = FALSE.
#' @param ... Additional parameters passed to `stats::dist()`
#' and `stats::cor()`
#' @return A CalmrRSA object
#' @note The object returned by this function
#' can be later tested via its own [test()] method.
#' @export
#' @examples
#' # Comparing the associations in three models
#' exp <- data.frame(
#'   Group = c("A", "B"),
#'   P1 = c("!2(A)>(US)/1B>(US)", "!1(A)>(US)/2B>(US)")
#' )
#' models <- c("HD2022", "RW1972", "PKH1982")
#' parameters <- sapply(models, get_parameters, design = exp)
#' exp_res <- compare_models(exp,
#'   models = models
#' )
#' comparisons <- list(
#'   "HD2022" = c("associations"),
#'   "RW1972" = c("associations"),
#'   "PKH1982" = c("associations")
#' )
#' res <- rsa(exp_res, comparisons = comparisons)
#' test(res, n_samples = 20)
rsa <- function(x, comparisons, test = FALSE, ...) {
  # Assert the comparisons list is named
  modnames <- names(comparisons)
  if (!length(modnames)) {
    stop("Comparisons list must be a model-named list.")
  }
  # Assert all model outputs are in x
  all_res <- lapply(x, results)

  sapply(modnames, function(m) {
    sapply(comparisons[[m]], function(o) {
      if (is.null(all_res[[m]][[o]])) {
        stop(sprintf("Output %s not found for model %s", o, m))
      }
    })
  })

  # Get trial matrices
  trial_mats <- sapply(modnames, function(m) {
    sapply(comparisons[[m]], function(o) {
      dat <- all_res[[m]][[o]]
      tsize <- unique(with(dat, tapply(value, trial, length)))
      if (length(tsize) > 1) {
        stop(sprintf("Output %s for model %s is inconsistent across trials.
        Cannot calculate distances", o, m))
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

  if (test) {
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
