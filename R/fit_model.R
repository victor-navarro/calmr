#' Fit model to data
#' @description Obtain MLE estimates for model, given data
#' @param data A numeric vector containing data to fit model against.
#' @param model_function A function that runs the model and returns data.frame of r-values, organized as data.
#' @param model_args The arguments to train the model function. Usually as returned by make_model_args.
#' @param optimizer_options A list with options for the optimizer, as returned by get_optimizer_opts.
#' @param file A path to save the model fit. If the arguments to the fit call are found to be identical to those in the file, the model just gets loaded.
#' @param ... Extra parameters passed to the optimizer call
#' @return A list with
#' \itemize{
#' \item {nloglik: the negative log-likelihood of the model}
#' \item {best_pars: the MLE parameters}
#' \item {model_pars: the model-specific MLE parameters}
#' \item {link_pars: the link-specific MLE parameters}
#' \item {data: the data used to fit the model}
#' \item {model_function: the model function supplied by the user}
#' \item {link_function: the link function used during the process}
#' \item {ll_function: the log-likelihood function used during the search process}
#' \item {model_args: the model function arguments supplied by the user}
#' \item {optimizer_options: the optimizer options supplied by the user}
#' \item {extra_pars: any extra parameters passed to the optimizer call via ...}
#' }
#' @note See the calmr_fits vignette for examples
#' @export
#' @seealso \code{\link{get_optimizer_opts}}, \code{\link{make_model_args}}
fit_model <- function(data, model_function, model_args, optimizer_options, file = NULL, ...){
  #check if the user passed lower and upper limits
  .calmr_check("limits")
  #check if user wants to save the fit in a file
  if (!is.null(file)) .calmr_check("filepath_OK", file)

  #split the parameters
  model_par_pointers = which(optimizer_options$all_pars %in% optimizer_options$model_pars)
  link_par_pointers = which(optimizer_options$all_pars %in% optimizer_options$family_pars)

  #get link function
  link_function = .get_calmr_link(optimizer_options$family)
  #get the log likelihood function
  ll_function = .get_calmr_loglikelihood(optimizer_options$family)

  #define the objective function
  objective_function = function(pars, ...){
    #generate model responses
    model_responses = model_function(pars[model_par_pointers],
                                     model_args = model_args,
                                     ...)
    #apply link
    model_responses = link_function(model_responses, pars[link_par_pointers])
    #get the likelihood
    model_likelihood = ll_function(data, model_responses)
    #return the negative sum of log likehood
    nll = -sum(model_likelihood)
    if (optimizer_options$verbose) cat("Parameters", format(pars, digits = 3), "\n")
    if (optimizer_options$verbose) cat("NLL:", nll, "\n")
    return(nll)
  }

  #check if the model has been run and can be loaded
  if (.check_fit_file(file)){
    return(readRDS(file))
  }

  if (optimizer_options$optimizer == 'optim'){
    #optimize
    opt_res = stats::optim(fn = objective_function,
                           lower = optimizer_options$ll,
                           upper = optimizer_options$ul,
                           ...)
    best_pars = opt_res$par
    best_nloglik = opt_res$value
  }

  if (optimizer_options$optimizer == "ga"){
    nobjective_function = function(...) -1*objective_function(...) #ga maximises, so must work with loglik
    opt_res = GA::ga(type = "real-valued",
                     fitness = nobjective_function,
                     lower = optimizer_options$ll,
                     upper = optimizer_options$ul,
                     ...)
    best_pars = as.numeric(opt_res@solution)
    best_nloglik = -1*opt_res@fitnessValue
    if (!is.null(nrow(best_pars))){
      warning("More than one solution yielded the best fit. Returning the first solution.")
      best_pars = best_pars[1, ]
    }
    names(best_pars) = unlist(optimizer_options[c('stim_names', 'family_pars')])
  }

  fit <- new("CalmrFit",
             nloglik = best_nloglik,
             best_pars = setNames(best_pars, optimizer_options$all_pars),
             model_pars = best_pars[model_par_pointers],
             link_pars = best_pars[link_par_pointers],
             data = data,
             model_function = model_function,
             link_function = link_function,
             ll_function = ll_function,
             model_args = model_args,
             optimizer_options = optimizer_options,
             extra_pars = list(...))

  if (!is.null(file)){
    saveRDS(fit, file)
  }
  fit


}