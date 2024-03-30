#' Get optimizer options
#' @param model_pars A character vector specifying the name of
#' the parameters to fit.
#' @param initial_pars A numeric vector specifying the initial
#' parameter values to #' evaluate the model at (required by `optim`).
#' Defaults to 0 for each parameter.
#' @param ll,ul A numeric vector specifying the lower and upper
#' limits of the parameters to fit, respectively
#' @param optimizer A string specifying the optimizer to use.
#' One from `c("optim", "ga")`
#' @param family A string specifying the family function to
#' generate responses (and calculate the likelihood function with).
#' One from `c("identity", "normal", "poisson")`.
#' @note Whenever a family function other than the identity is used,
#' the family-specific parameters will always be appended to
#' the end of the relevant lists.
#' @seealso [fit_model()]
#' @export
#' @return A list with optimizer options.
get_optimizer_opts <- function(model_pars,
                               initial_pars = rep(NA, length(model_pars)),
                               ll = rep(NA, length(model_pars)),
                               ul = rep(NA, length(model_pars)),
                               optimizer = NULL, family = NULL) {
  optimizer <- .assert_optimizer(optimizer)
  family <- .assert_family(family)

  # family-specific
  family_pars <- NULL
  if (family %in% c("normal", "poisson")) {
    family_pars <- paste0(family, "_scale")
    initial_pars <- c(initial_pars, 1)
    ll <- c(ll, 0)
    ul <- c(ul, 100)
  }
  # some naming
  all_pars <- c(model_pars, family_pars)
  names(ll) <- names(ul) <- all_pars

  list(
    model_pars = model_pars,
    optimizer = optimizer,
    family = family,
    family_pars = family_pars,
    all_pars = all_pars,
    initial_pars = initial_pars,
    ll = ll,
    ul = ul,
    verbose = FALSE
  )
}

.get_calmr_link <- function(family) {
  link_f <- NULL
  if (family %in% c("identity")) {
    link_f <- function(y, c) y
  }
  if (family == "normal") {
    link_f <- function(y, c) y * c
  }
  if (family == "poisson") {
    link_f <- function(y, c) exp(y * c)
  }
  if (is.null(link_f)) {
    stop(sprintf(
      "Couldn't find a link function when using family %s", family
    ))
  }
  link_f
}

.get_calmr_loglikelihood <- function(family) {
  like_f <- NULL
  if (family %in% c("identity", "normal")) {
    like_f <- function(dat, mod) stats::dnorm(dat - mod, log = TRUE)
  }
  if (family == "poisson") {
    like_f <- function(dat, mod) stats::dpois(dat, mod + 1e-9, log = TRUE)
  } # note the adjustment, the poisson needs positive rates
  if (is.null(like_f)) {
    stop(sprintf("Couldn't find a likelihood function when
    using family %s", family))
  }
  like_f
}

.check_fit_file <- function(file) {
  if (is.null(file)) {
    return(FALSE)
  }
  if (!file.exists(file)) {
    return(FALSE)
  }
  fit <- readRDS(file)
  args <- parent.frame()
  tests <- c(
    isTRUE(all.equal(args$data, fit@data)),
    isTRUE(all.equal(args$model_function, fit@model_function)),
    isTRUE(all.equal(args$link_function, fit@link_function)),
    isTRUE(all.equal(args$ll_function, fit@ll_function)),
    isTRUE(all.equal(args$optimizer_options, fit@optimizer_options))
  )
  all(tests)
}
