#' Obtain model parameters
#' @param design An data.frame containing the experimental design.
#' @param model A string specifying a model. One in supported_models()
#' @seealso \code{\link{supported_models}}
#' @export

get_model_params <- function(design, model = NULL){
  if (is.null(model)){
    model = .calmr_default("model_name")
  }
  parsed_design = parse_design(design)

  stims = parsed_design %>%
    tidyr::unnest_wider(.data$trial_info) %>%
    dplyr::select(.data$unique_nominal_stimuli) %>%
    unlist() %>%
    unique()

  if (model %in% c("HDI2020", "HD2022", "RAND")){
    df = data.frame(stimulus = stims,
                    alphas = 0.2)
  }
  if (model %in% c("RW1972")){
    df = data.frame(stimulus = stims,
                    alphas = 0.2,
                    betas_on = 0.2,
                    betas_off = 0.2,
                    lambdas = 1)
  }
  if (model %in% c("MAC1975")){
    df = data.frame(stimulus = stims,
                    alphas = 0.2,
                    min_alphas = 0.1,
                    max_alphas = 1.0,
                    betas_on = 0.2,
                    betas_off = 0.2,
                    lambdas = 1,
                    thetas = .2,
                    gammas = 1/length(stims))
  }
  if (model %in% c("PKH1982")){
    df = data.frame(stimulus = stims,
                    alphas = 0.2,
                    min_alphas = 0.1,
                    max_alphas = 1.0,
                    betas_ex = .2,
                    betas_in = .1,
                    lambdas = 1,
                    thetas = 1,
                    gammas = 1/length(stims))
  }
  df
}
