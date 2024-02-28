#' Miscellaneous information functions
#' @param model A string specifying a model. One from `supported_models()`
#' @description
#' \describe{
#' \item{\code{supported_models}}{Returns the models supported in the package.}
#' \item{\code{supported_plots, model_outputs}}{Returns the plots, outputs
#' supported by argument `model`.}
#' \item{\code{supported_families}}{Returns the families supported by
#' \code{\link{fit_model}}.}
#' \item{\code{supported_optimizers}}{returns the optimizers
#' supported by \code{\link{fit_model}}.}
#' \item{\code{supported_plots}}{returns the plots supported by
#' argument `model`.}
#' \item{\code{get_model}}{Returns the function specified by argument `model`.}
#' \item{\code{parameter_info}}{Returns parameter information for a
#' `model` (or all of them if model = NULL)}
#' \item{\code{}}{}
#' }
#' @name information_functions
#' @rdname model_info
#' @export
supported_models <- function() {
  c(
    "HDI2020", "HD2022", "RW1972", "MAC1975",
    "PKH1982", "SM2007", "RAND", "ANCCR"
  )
}

#' @rdname model_info
#' @export
supported_optimizers <- function() {
  c("optim", "ga")
}

#' @rdname model_info
#' @export
supported_families <- function() {
  c("identity", "normal", "poisson", "OLS")
}

#' @rdname model_info
#' @export
supported_plots <- function(model = NULL) {
  plot_info <- list(
    "HDI2020" = c("as", "acts", "rs", "vs"),
    "HD2022" = c("as", "acts", "rs", "vs"),
    "RW1972" = c("rs", "vs"),
    "MAC1975" = c("as", "rs", "vs"),
    "SM2007" = c("acts", "relacts", "vs", "os"),
    "PKH1982" = c("as", "rs", "eivs"),
    "ANCCR" = c(
      "e_ij", "e_i", "m_i", "delta", "m_ij",
      "psrcs", "ncs", "anccrs", "cws", "das", "qs"
    ),
    "RAND" = c("rs", "vs")
  )
  if (is.null(model)) {
    plot_info
  } else {
    model <- .calmr_assert("supported_model", model)
    plot_info[[model]]
  }
}

#' @rdname model_info
#' @export
get_model <- function(model) {
  # Check model is supported
  .calmr_assert("supported_model", model)
  get(model)
}


# Internal function to get parameter names
.get_model_parnames <- function(model) {
  # TODO: Hard-code this information
  # TODO: Write test
  pars <- get_parameters(
    design = data.frame(g = "X", p1 = "X", r1 = TRUE),
    model = model
  )
  names(pars)[-1]
}

parameter_info <- function(model = NULL) {
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
        "transition_delay", "post_trial_delay",
        "mean_ITI", "max_ITI", "reward_magnitude",
        "betas", "thresholds", "ks",
        "w", "minimum_rate", "sampling_interval",
        "use_exact_mean", "use_exponential",
        "t_ratio", "t_constant", "t_jitter",
        "alpha", "alpha_reward", "use_timed_alpha",
        "alpha_exponent", "alpha_init", "alpha_min"
      ),
      default_value = c(
        1, 1,
        30, 90, 1,
        1, 0.6, 1,
        0.5, 1e-3, 0.2,
        FALSE, TRUE,
        1.2, NA, 0.1,
        0.02, 0.2, FALSE,
        1, 1, 0
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
      "w", "minimum_rate",
      "sampling_interval",
      "use_exact_mean", "use_exponential",
      "t_ratio", "t_constant", "t_jitter",
      "alpha", "alpha_reward", "use_timed_alpha",
      "alpha_exponent", "alpha_init", "alpha_min"
    )
  )
  parameter %in% global_pars[[model]]
}

# Returns whether a parameter is a trial parameter
.is_trial_parameter <- function(parameter, model) {
  trial_pars <- list(
    "ANCCR" = c(
      "post_trial_delay",
      "mean_ITI", "max_ITI"
    )
  )
  parameter %in% trial_pars[[model]]
}

# Returns wheter a parameter is a transition parameter
.is_trans_parameter <- function(parameter, model) {
  trans_pars <- list(
    "ANCCR" = c(
      "transition_delay"
    )
  )
  parameter %in% trans_pars[[model]]
}

#' @rdname model_info
#' @export
model_outputs <- function(model = NULL) {
  output_info <- list(
    "HDI2020" = c("as", "acts", "rs", "vs"),
    "HD2022" = c("as", "acts", "rs", "vs"),
    "RW1972" = c("rs", "vs"),
    "MAC1975" = c("as", "rs", "vs"),
    "SM2007" = c("acts", "relacts", "vs", "os"),
    "PKH1982" = c("as", "rs", "eivs"),
    "ANCCR" = c(
      "e_ij", "e_i", "m_i", "delta", "m_ij",
      "psrcs", "ncs", "anccrs", "cws", "das", "qs"
    ),
    "RAND" = c("rs", "vs")
  )
  if (is.null(model)) {
    output_info
  } else {
    model <- .calmr_assert("supported_model", model)
    output_info[[model]]
  }
}

# defining where the associations are in each model
.model_associations <- function(model) {
  assoc_map <- c(
    "HDI2020" = "vs",
    "HD2022" = "vs",
    "RW1972" = "vs",
    "MAC1975" = "vs",
    "SM2007" = "vs",
    "PKH1982" = "eivs",
    "ANCCR" = "anccrs",
    "RAND" = "vs"
  )
  assoc_map[model]
}
