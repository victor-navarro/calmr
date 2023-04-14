#' Miscellaneous information functions
#' `supported_models` returns the models supported in the package.
#' `supported_plots` returns the plots supported by argument `model`.
#' `supported_families` returns the families supported by \code{\link{fit_model}}.
#' `supported_optimizers` returns the optimizers supported by \code{\link{fit_model}}.
#' `suported_plots` returns the plots supported by argument `model`.
#' @param model A string specifying a model. One from `supported_models()`
#' @rdname model_info_funcs

#' @export
supported_models <- function(){
  c("HDI2020", "HD2022", "RW1972", "MAC1975", "PKH1982", "SM2007", "RAND")
}

#' @export
supported_optimizers <- function(){
  c("optim", "ga")
}

#' @export
supported_families <- function(){
  c("identity", "normal", "poisson", "OLS")
}

#' @export
supported_plots <- function(model = NULL){
  plot_info = list(
    "HDI2020" = c("as", "acts", "rs", "vs"),
    "HD2022" = c("as", "acts", "rs", "vs"),
    "RW1972" = c("es", "vs"),
    "MAC1975" = c("as", "es", "vs"),
    "SM2007" = c("acts", "vs"),
    "PKH1982" = c("as", "es", "eivs"),
    "RAND" = c("es", "vs")
  )
  if (is.null(model)){
    plot_info
  }else{
    .calmr_check(model, "supported_model")
    plot_info[[model]]
  }
}

#' @rdname model_info_funcs
.get_model_parnames <- function(model){
  #check if model is supported
  .calmr_check(model, "supported_model")
  pars = get_model_params(design = data.frame(g = "X", p1 = "X", r1 = T),
                          model = model)
  names(pars)[-1]
}


.calmr_check <- function(type, given = NULL, necessary = NULL){
  switch(type,
         "supported_model" = {
           if (!given %in% supported_models())
             stop("Model is not supported. Must be one returned by supported_models()", call. = F)
         },
         "params_OK" = {
           if ((length(setdiff(given$stimulus, necessary$stimulus)) +
                length(setdiff(necessary$stimulus, given$stimulus))) > 0)
             stop("Found mismatch between user supplied stimulus names and those in the experimental design. See ?get_model_params", call. = F)
           if (any(!(names(necessary) %in% names(given))))
             stop("The parameter data.frame does not meet the parameter requirements. See ?get_model_params", call. = F)
         },
         "options_OK" = {
           if (length(setdiff(names(given), names(given))) > 0)
             stop("Did not supply proper options. Please see ?get_exp_opts", call. = F)
         },
         "supported_optimizer" = {
           if (!given %in% supported_optimizers())
             stop("Optimizer is not supported. Must be one returned by supported_optimizers()", call. = F)
         },
         "supported_family" = {
           if (!given %in% supported_families())
             stop("Family is not supported. Must be one returned by supported_families()", call. = F)
         },
         "limits_OK" = {
           if (any(is.na(given$ll)) | any(is.na(given$ul)))
             stop("Did not supply limits for all parameters estimated. Please see ?fit_model", call. = F)
         },
         "filepath_OK" = {
           if (!file.exists(dirname(given)))
             stop(sprintf("Path to file (%s) does not exist.", given), call. = F)
         },
         "no_functional_stimuli" = {
           if (length(given$unique_nominal_stimuli) > length(given$unique_functional_stimuli)){
             stop("The model does not support functional/nominal stimuli specifications.", call. = F)
           }
         },
         "supported_plot" = {
           if (!(given %in% necessary)){
             stop(sprintf("Plot not supported. The model does not contain '%s' in model results.", given), call. = F)
           }
         },
         "good_experiment" = {
           if (any(unlist(lapply(given$mapping, function(x) length(x$unique_nominal_stimuli))) == 1)){
             stop("Experiment is too simple to run for one or more groups. Please check your design.", call. = F)
           }

         },
         "comparator_order" = {
           if (unique(given)>1){
             stop("Multiple orders for comparison process are not currently supported. Please make sure the orders column only contains one value.")
           }
         }



  )
}

.calmr_default <- function(type, ...){
  switch(type,
         "model_name" = {
           warning("No model passed. Using 'HD2022'.", call. = F)
           return("HD2022")},
         "model_params" = {
           warning("No parameters passed. Using default values.", call. = F)
           return(get_model_params(...))
         },
         "experiment_options" = {
           warning("No experiment options passed. Using default options.", call. = F)
           return(get_exp_opts())
         },
         "optimizer" = {
           warning("No optimizer passed. Using 'optim'.", call. = F)
           return("optim")
         },
         "family" = {
           warning("No family passed. Using 'identity'", call. = F)
           return("identity")
         }
  )
}
