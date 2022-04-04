#' An assortment of functions to help fit HeiDI
#' @param optimizer A string specifying the optimizer to use. One from `c("optim")`
#' @param stim_names A string specifying the name of stimuli in the experiment
#' @param family A string specifying the family function to generate responses (and calculate MLE with). Currently supports `c("identity", "linear", "poisson")`
#' @param adj The adjustment factor for upper and lower bounds. Default is 1e-6
#' @note Whenever a family function other than the identity is used, the family-specific parameters will always be appended to the end of the relevant list entries.
#' @rdname fit_helpers
#' @export
get_optimizer_opts <- function(optimizer, stim_names, family, adj = 1e-6){
  npars = length(stim_names)
  par_ops = list(stim_names = stim_names,
                 lower = rep(0+adj, npars),
                 upper = rep(1-adj, npars),
                 optimizer = optimizer,
                 sample_pars = function() stats::rbeta(npars, 10, 20),  #biased around .3, could change
                 family = family,
                 family_pars = c(),
                 verbose = F)
  #family-specific
  if (family %in% c("linear", "poisson")){
    par_ops$lower = c(par_ops$lower, 0+adj)
    par_ops$upper = c(par_ops$upper, 100) #arbitrary
    par_ops$sample_pars = function() c(stats::rbeta(npars, 10, 20), stats::rgamma(1, 2))
    par_ops$family_pars = "scale"
  }
  #optimizer-specific parameters go here
  if (optimizer == 'optim'){
    par_ops$optim_options = list(method = "L-BFGS-B",
                                 control = list(trace = 3))
  }
  par_ops
}

#' @rdname fit_helpers
#' @export
fit_predict <- function(fit, new_args = NULL, type = "response"){
  if (!is.null(new_args)){
    predict_args = new_args
  }else{
    predict_args = fit$model_args
  }
  prediction = fit$model_function(fit$model_pars, predict_args)
  if (type == "response"){
    prediction$value = fit$link_function(prediction$value, fit$link_pars)
  }
  prediction
}

plot_fit <- function(fit){
  prediction = fit_predict(fit)
  prediction$data = fit$data
  prediction %>% rename("prediction" = "value") %>%
    tidyr::pivot_longer(cols = c("prediction", "data"),
                        names_to = "type",
                        values_to = "value") %>%
    ggplot2::ggplot(ggplot2::aes(x = trial, y = value, linetype = type)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()
}

.get_heidi_link <- function(family){
  link_f = NULL
  if (family == "identity"){link_f = function(y, c) y}
  if (family == "linear"){link_f = function(y, c) y*c}
  if (family == "poisson"){link_f = function(y, c) exp(y*c)}
  if (is.null(link_f)) stop(sprintf("Couldn't find a link function when using family %s", family))
  link_f
}

.get_heidi_loglikelihood <- function(family){
  like_f = NULL
  if (family %in% c("identity", "linear")) {like_f = function(dat, mod) stats::dnorm(dat-mod, log = T)}
  if (family =="poisson"){like_f = function(dat, mod) stats::dpois(dat, mod+1e-9, log = T)} #note the adjustment, the poisson needs positive rates
  if (is.null(like_f)) stop(sprintf("Couldn't find a likelihood function when using family %s", family))
  like_f
}
