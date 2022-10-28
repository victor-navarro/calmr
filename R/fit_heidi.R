#' Fit HeiDI to data
#' @description Obtain MLE estimates for HeiDI, given data
#' @param data A numeric vector containing data to fit HeiDI against.
#' @param model_function A function that runs the model and returns data.frame of r-values, organized as data.
#' @param model_args The arguments to train the model function. Usually as returned by make_model_args.
#' @param optimizer_options A list with options for the optimizer, as returned by get_optimizer_opts.
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
#' @note See the fitting_heidi vignette for examples
#' @export
#' @seealso \code{\link{get_optimizer_opts}}, \code{\link{make_model_args}}
fit_heidi <- function(data, model_function, model_args, optimizer_options, ...){
  npars = length(optimizer_options$lower)
  #determine where to the model-specific parameters end
  model_par_pointers = 1:npars
  link_par_pointers = NA
  if (optimizer_options$family %in% c("linear", "poisson")){
    model_par_pointers = 1:(npars-1)
    link_par_pointers = npars
  }
  #get link function
  link_function = .get_heidi_link(optimizer_options$family)
  #get the log likelihood function
  ll_function = .get_heidi_loglikelihood(optimizer_options$family)
  #define the objective function
  objective_function = function(pars, ...){
    #generate model responses
    model_responses = model_function(pars[model_par_pointers], model_args = model_args)$value
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
  if (optimizer_options$optimizer == 'optim'){
    #optimize
    opt_res = stats::optim(par = optimizer_options$sample_pars(),
                           fn = objective_function,
                           method = optimizer_options$optim_options$method,
                           lower = optimizer_options$lower,
                           upper = optimizer_options$upper,
                           control = optimizer_options$optim_options$control,
                           ...)
    best_pars = opt_res$par
    names(best_pars) = unlist(optimizer_options[c('stim_names', 'family_pars')])
    best_nloglik = opt_res$value
  }
  results = list(nloglik = best_nloglik,
                 best_pars = best_pars,
                 model_pars = best_pars[model_par_pointers],
                 link_pars = best_pars[link_par_pointers],
                 data = data,
                 model_function = model_function,
                 link_function = link_function,
                 ll_function = ll_function,
                 model_args = model_args,
                 optimizer_options = optimizer_options,
                 extra_pars = list(...))
  return(results)
}
