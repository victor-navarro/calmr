#' Fit model to data
#' @description Obtain MLE estimates for model, given data.
#' @param data A numeric vector containing data to fit model against.
#' @param model_function A function that runs the model and
#' returns data.frame of value, organized as in `data`.
#' @param optimizer_options A list with options for the
#' optimizer, as returned by [get_optimizer_opts].
#' @param file A path to save the model fit. If the arguments
#' to the fit call are found to be identical to those in the file,
#' the model just gets loaded.
#' @param ... Extra parameters passed to the optimizer call.
#' @return A [CalmrFit-class] object
#' @note See the calmr_fits vignette for examples
#' @export
#' @seealso [get_optimizer_opts()]
#' @examples
#' # Make some fake data
#' df <- data.frame(g = "g", p1 = "!3A>(US)")
#' pars <- get_parameters(df, model = "RW1972")
#' pars$alphas["US"] <- 0.9
#' exper <- make_experiment(df, parameters = pars, model = "RW1972")
#' res <- run_experiment(exper, outputs = "responses")
#' responses <- results(res)$responses$value
#'
#' # define model function
#' model_fun <- function(p, ex) {
#'   np <- parameters(ex)
#'   np[[1]]$alphas[] <- p
#'   parameters(ex) <- np
#'   results(run_experiment(ex))$responses$value
#' }
#'
#' # Get optimizer options
#' optim_opts <- get_optimizer_opts(
#'   model_pars = names(pars$alphas),
#'   ll = rep(.05, 2), ul = rep(.95, 2),
#'   optimizer = "optim", family = "identity"
#' )
#' optim_opts$initial_pars[] <- rep(.6, 2)
#'
#' fit_model(responses, model_fun, optim_opts,
#'   ex = exper, method = "L-BFGS-B",
#'   control = list(maxit = 1)
#' )
fit_model <- function(
    data, model_function,
    optimizer_options, file = NULL, ...) {
  # check if the user passed lower and upper limits
  .assert_limits(optimizer_options)
  # check if user wants to save the fit in a file
  if (!is.null(file)) .assert_filepath(file)

  # split the parameters
  model_par_pointers <- which(
    optimizer_options$all_pars %in% optimizer_options$model_pars
  )
  link_par_pointers <- which(
    optimizer_options$all_pars %in% optimizer_options$family_pars
  )

  # get link function
  link_function <- .get_calmr_link(optimizer_options$family)
  # get the log likelihood function
  ll_function <- .get_calmr_loglikelihood(optimizer_options$family)

  # define the objective function
  objective_function <- function(pars, ...) {
    # generate model responses
    model_responses <- model_function(
      pars[model_par_pointers],
      ...
    )
    # apply link
    model_responses <- link_function(model_responses, pars[link_par_pointers])
    # get the likelihood
    model_likelihood <- ll_function(data, model_responses)
    # return the negative sum of log likehood
    nll <- -sum(model_likelihood)
    if (optimizer_options$verbose) {
      message("Parameters", format(pars, digits = 3), "\n", "NLL:", nll, "\n")
    }
    return(nll)
  }

  # check if the model has been run and can be loaded
  if (.check_fit_file(file)) {
    return(readRDS(file))
  }

  if (optimizer_options$optimizer == "optim") {
    # optimize
    opt_res <- stats::optim(
      par = optimizer_options$initial_pars,
      fn = objective_function,
      lower = optimizer_options$ll,
      upper = optimizer_options$ul,
      ...
    )
    best_pars <- opt_res$par
    best_nloglik <- opt_res$value
  }

  if (optimizer_options$optimizer == "ga") {
    nobjective_function <- function(...) {
      -1 * objective_function(...)
    } # ga maximises, so must work with loglik
    opt_res <- GA::ga(
      type = "real-valued",
      fitness = nobjective_function,
      lower = optimizer_options$ll,
      upper = optimizer_options$ul,
      ...
    )
    best_pars <- as.numeric(opt_res@solution)
    best_nloglik <- -1 * opt_res@fitnessValue
    if (!is.null(nrow(best_pars))) { # nocov start
      warning("More than one solution yielded the best fit.
      Returning the first solution.")
      best_pars <- best_pars[1, ]
    } # nocov end
    names(best_pars) <- unlist(
      optimizer_options[c("stim_names", "family_pars")]
    )
  }

  fit <- methods::new("CalmrFit",
    nloglik = best_nloglik,
    best_pars = stats::setNames(best_pars, optimizer_options$all_pars),
    model_pars = best_pars[model_par_pointers],
    link_pars = best_pars[link_par_pointers],
    data = data,
    model_function = model_function,
    link_function = link_function,
    ll_function = ll_function,
    optimizer_options = optimizer_options,
    extra_pars = list(...)
  )

  if (!is.null(file)) {
    saveRDS(fit, file)
  }
  fit
}
