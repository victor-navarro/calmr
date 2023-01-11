#' Miscellaneous information functions
#' `supported_models` returns the models supported in the package.
#' `supported_plots` returns the plots supported by argument `model`.
#' @param model A string specifying a model. One in `supported_models()`
#' @rdname model_info_funcs

#' @export
supported_models <- function(){
  c("HDI2020", "HD2022", "RW1972","RAND")
}

#' @export
supported_optimizers <- function(){
  c("optim", "ga")
}

#' @export
supported_families <- function(){
  c("identity", "normal", "poisson")
}

#' @rdname model_info_funcs
#' @export
supported_plots <- function(model = NULL){
  plot_info = list(
    "HDI2020" = c("as", "acts", "rs", "vs"),
    "HD2022" = c("as", "acts", "rs", "vs"),
    "RW1972" = c("es", "vs"),
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
  pars = get_model_params(design =data.frame(g = "X", p1 = "X", r1 = T),
                          model = model)
  names(pars)[-1]
}


.calmr_check <- function(type, given = NULL, necessary = NULL){
  switch(type,
         "supported_model" = {
           if (!given %in% supported_models())
             stop("Model is not supported. Must be one returned by supported_models()")
         },
         "params_OK" = {
           if ((length(setdiff(given$stimulus, necessary$stimulus)) +
                length(setdiff(necessary$stimulus, given$stimulus))) > 0)
             stop("Found mismatch between user supplied stimulus names and those in the experimental design. See ?get_model_params")
           if (any(!(names(necessary) %in% names(given))))
             stop("The parameter data.frame does not meet the parameter requirements. See ?get_model_params")
         },
         "options_OK" = {
           if (length(setdiff(names(given), names(given))) > 0)
             stop("Did not supply proper options. Please see ?get_exp_opts")
         },
         "supported_optimizer" = {
           if (!given %in% supported_optimizers())
             stop("Optimizer is not supported. Must be one returned by supported_optimizers()")
         },
         "supported_family" = {
           if (!given %in% supported_families())
             stop("Family is not supported. Must be one returned by supported_families()")
         },
         "limits_OK" = {
           if (any(is.na(given$ll)) | any(is.na(given$ul)))
             stop("Did not supply limits for all parameters estimated. Please see ?fit_model")
         },
         "filepath_OK" = {
           if (!file.exists(dirname(given)))
             stop(sprintf("Path to file (%s) does not exist.", given))
         }

  )
}

.calmr_default <- function(type, ...){
  switch(type,
         "model_name" = {
           warning("No model passed. Using 'HD2022'.")
           return("HD2022")},
         "model_params" = {
           warning("No parameters passed. Using default values.")
           return(get_model_params(...))
         },
         "experiment_options" = {
           warning("No experiment options passed. Using default options.")
           return(get_exp_opts())
         },
         "optimizer" = {
           warning("No optimizer passed. Using 'optim'.")
           return("optim")
         },
         "family" = {
           warning("No family passed. Using 'identity'")
           return("identity")
         }
  )
}
