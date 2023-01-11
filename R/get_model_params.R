#' Obtain model parameters
#' @param design An data.frame containing the experimental design.
#' @param model A string specifying a model. One in supported_models()
#' @seealso \code{\link{supported_models}}
#' @export

get_model_params <- function(design, model = NULL){
  if (is.null(model)){
    model = .calmr_default("model_name")
  }
  design = parse_design(design)
  stims = design %>%
    tidyr::unnest_wider(.data$trial_info) %>%
    dplyr::select(.data$unique_nominal_stimuli) %>%
    unlist() %>%
    unique()

  if (model %in% c("HDI2020", "HD2022", "RAND")){
    df = data.frame(stimulus = stims, alphas = 0.2)
  }
  if (model %in% c("RW1972")){
    df = data.frame(stimulus = stims, alphas = 0.2, betas_on = 0.2, betas_off = 0.1, lambdas = 1)
  }
  if (model %in% c("MAC1975")){
    df = data.frame(stimulus = stims, alphas = 0.2, lambdas = 1)
  }
  df
}
