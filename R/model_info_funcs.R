#' Miscellaneous model functions
#' `supported_models` returns the models supported in the package.
#' `supported_plots` returns the plots supported by argument `model`.
#' @param model A string specifying a model. One in `supported_models()`
#' @rdname model_info_funcs
#' @export

supported_models <- function(){
  c("HDI2020", "HD2022", "RW1972","RAND")
}

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

  if (model %in% c("HDI2020", "HD2022")){
    parnames = c("alphas")
  }
  if (model %in% c("RAND", "RW1972", "MAC1975")){
    parnames = c("alphas", "lambdas")
  }
  parnames
}


.calmr_check <- function(type, given = NULL, necessary = NULL){
  switch(type,
         "supported_model" = {
           if (!given %in% supported_models()) stop(.calmr_error("unsupported_model"))
         },
         "params_OK" = {
           if ((length(setdiff(given$stimulus, necessary$stimulus)) +
               length(setdiff(necessary$stimulus, given$stimulus))) > 0) stop(.calmr_error("bad_parameters_stimuli"))
           if (any(!(names(necessary) %in% names(given)))) stop(.calmr_error("bad_parameters_structure"))
         },
         "options_OK" = {
           if (length(setdiff(names(given), names(given))) > 0) stop(.calmr_error("bad_options"))
         }
  )
}

.calmr_error <- function(type){
  switch(type,
         "unsupported_model" = "Model is not supported. Must be one returned by supported_models()",
         "bad_parameters_stimuli" = "Found mismatch between user supplied stimulus names and those in the experimental design. See ?get_params",
         "bad_parameters_structure" = "The parameter data.frame does not meet the parameter requirements. See ?get_params",
         "bad_options" = "Did not supply proper options. Please see ?get_model_opts")
}
