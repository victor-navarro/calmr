#' Model information functions
#' @param model A string specifying a model. One from `supported_models()`.
#' @description An assortment of functions to return model information.
#' @name model_information
#'
#' @examples
#' # Outputs and plots supported by the RW1972 model
#' model_outputs("RW1972")
#'
#' # Getting the model function implementing the PKH1982 model
#' pkh_func <- get_model("PKH1982")
#' head(pkh_func, 10)
#'
#' # Getting the parameters required by SM2007
#' model_parameters("SM2007")
NULL
#> NULL

#' @rdname model_information
#' @return `supported_models()` returns a character vector.
#' @export
supported_models <- function() {
  c(
    "HDI2020", "HD2022", "RW1972", "MAC1975",
    "PKH1982", "SM2007", "RAND", "ANCCR",
    "TD"
  )
}
#' @rdname model_information
#' @return `supported_timed_models()` returns a character vector.
#' @export
supported_timed_models <- function() {
  c("ANCCR", "TD")
}

#' @rdname model_information
#' @return `supported_optimizers()` returns a character vector.
#' @export
supported_optimizers <- function() {
  c("optim", "ga")
}

#' @rdname model_information
#' @return `supported_families()` returns a character vector.
#' @export
supported_families <- function() {
  c("identity", "normal", "poisson")
}

#' @rdname model_information
#' @return `supported_plots()` returns a character vector or list
#' (if model is NULL).
#' @export
supported_plots <- function(model = NULL) {
  plot_info <- model_outputs()
  if (is.null(model)) {
    plot_info
  } else {
    model <- .assert_model(model)
    plot_info[[model]]
  }
}

#' @rdname model_information
#' @return `get_model()` returns a model function.
#' @export
get_model <- function(model) {
  # Check model is supported
  .assert_model(model)
  get(model)
}

#' @rdname model_information
#' @return `model_parameters()` returns a list or a
#' list of lists (if model is NULL).
#' @export
model_parameters <- function(model = NULL) {
  parameter_map <- list(
    "HDI2020" = list(
      name = c("alphas"),
      default_value = c(0.4)
    ),
    "HD2022" = list(
      name = c("alphas"),
      default_value = c(0.4)
    ),
    "RW1972" = list(
      name = c("alphas", "betas_on", "betas_off", "lambdas"),
      default_value = c(0.4, 0.4, 0.4, 1)
    ),
    "MAC1975" = list(
      name = c(
        "alphas", "min_alphas", "max_alphas",
        "betas_on", "betas_off", "lambdas", "thetas", "gammas"
      ),
      default_value = c(0.4, 0.1, 1.0, 0.4, 0.4, 1, .2, 0.3)
    ),
    "SM2007" = list(
      name = c(
        "alphas", "lambdas", "omegas", "rhos",
        "gammas", "taus", "order"
      ),
      default_value = c(0.4, 1, 0.2, 1, 1, 0.2, 1)
    ),
    "PKH1982" = list(
      name = c(
        "alphas", "min_alphas", "max_alphas",
        "betas_ex", "betas_in", "lambdas", "thetas", "gammas"
      ),
      default_value = c(0.4, 0.1, 1.0, 0.4, 0.3, 1, 1, 0.3)
    ),
    "ANCCR" = list(
      name = c(
        "reward_magnitude",
        "betas", "cost", "temperature",
        "threshold", "k",
        "w", "minimum_rate", "sampling_interval",
        "use_exact_mean",
        "t_ratio", "t_constant",
        "alpha", "alpha_reward", "use_timed_alpha",
        "alpha_exponent", "alpha_init", "alpha_min",
        "add_beta", "jitter"
      ),
      default_value = c(
        1,
        1, 0, 1,
        0.6, 1,
        0.5, 1e-3, 0.2,
        FALSE,
        1.2, NA,
        0.02, 0.2, FALSE,
        1, 1, 0,
        FALSE, 1
      )
    ),
    "TD" = list(
      name = c(
        "alphas", "betas_on", "betas_off",
        "lambdas", "gamma", "sigma"
      ),
      default_value = c(
        0.05, 0.4, 0.4,
        1, 0.95, 0.90
      )
    ),
    "RAND" = list(
      name = c("alphas"),
      default_value = c(0.4)
    )
  )
  if (is.null(model)) {
    return(parameter_map)
  } else {
    return(parameter_map[[model]])
  }
}


# Returns whether a parameter is a global parameter
.is_global_parameter <- function(parameter, model) {
  global_pars <- list(
    "SM2007" = c("order"),
    "ANCCR" = c(
      "cost", "temperature",
      "threshold", "k",
      "w", "minimum_rate",
      "sampling_interval",
      "use_exact_mean",
      "t_ratio", "t_constant",
      "alpha", "alpha_reward", "use_timed_alpha",
      "alpha_exponent", "alpha_init", "alpha_min",
      "add_beta", "jitter"
    ),
    "TD" = c("gamma", "sigma")
  )
  parameter %in% global_pars[[model]]
}

#' @rdname model_information
#' @return `model_outputs()` returns a character vector or
#' list (if model is NULL).
#' @export
model_outputs <- function(model = NULL) {
  output_info <- list(
    "HDI2020" = c("activations", "associations", "pools", "responses"),
    "HD2022" = c("activations", "associations", "pools", "responses"),
    "RW1972" = c("associations", "responses"),
    "MAC1975" = c("associabilities", "associations", "responses"),
    "SM2007" = c(
      "activations", "associations",
      "relative_activations", "operator_switches"
    ),
    "PKH1982" = c("associabilities", "associations", "responses"),
    "ANCCR" = c(
      "action_values", "anccrs",
      "causal_weights", "dopamines",
      "ij_eligibilities", "i_eligibilities",
      "i_base_rate", "ij_base_rate",
      "net_contingencies", "probabilities",
      "representation_contingencies"
    ),
    "TD" = c("values", "associations", "eligibilities"),
    "RAND" = c("associations", "responses")
  )
  if (is.null(model)) {
    output_info
  } else {
    model <- .assert_model(model)
    output_info[[model]]
  }
}

# defining where the associations are in each model
.model_associations <- function(model) {
  assoc_map <- c(
    "HDI2020" = "associations",
    "HD2022" = "associations",
    "RW1972" = "associations",
    "MAC1975" = "associations",
    "SM2007" = "associations",
    "PKH1982" = "associations",
    "ANCCR" = "anccrs",
    "TD" = "associations",
    "RAND" = "associations"
  )
  assoc_map[model]
}
