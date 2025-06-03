#' Model information functions
#' @param model A string specifying a model. One from `supported_models()`.
#' @description An assortment of functions to return model information.
#' @name model_information
#'
#' @examples
#' # Outputs and plots supported by the RW1972 model
#' model_outputs("RW1972")
#'
#' # Getting a model instance of the PKH1982 model
#' pkh_inst <- get_model("PKH1982")
#'
#' # Getting the `run` method for the MAC1975
#' head(methods::getMethod("run", "MAC1975"), 10)
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
    "RW1972", "HDI2020", "HD2022",
    "MAC1975", "PKH1982", "RAND",
    "SM2007", "TD", "ANCCR"
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
#' @return `get_model()` returns a [CalmrModel-class] model instance.
#' @export
get_model <- function(model) {
  # Check model is supported
  .assert_model(model)
  methods::new(model)
}

#' @rdname model_information
#' @return `model_parameters()` returns a list or a
#' list of lists (if model is NULL).
#' @examples
#' model_parameters("RW1972")
#' @export
model_parameters <- function(model = NULL) {
  parameter_map <- lapply(
    supported_models(),
    function(m) methods::new(m)@default_parameters
  )
  names(parameter_map) <- supported_models()
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
  output_info <- lapply(
    supported_models(),
    function(m) methods::new(m)@outputs
  )
  names(output_info) <- supported_models()
  if (is.null(model)) {
    output_info
  } else {
    model <- .assert_model(model)
    output_info[[model]]
  }
}

# defining where the associations are in each model
.model_associations <- function(model) {
  assoc_map <- lapply(
    supported_models(),
    function(m) methods::new(m)@.associations
  )
  names(assoc_map) <- supported_models()
  assoc_map[[model]]
}
