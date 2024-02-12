#' Miscellaneous information functions
#' @param model A string specifying a model. One from `supported_models()`
#' @note
#' `supported_models` returns the models supported in the package.
#' `supported_plots` returns the plots supported by argument `model`.
#' `supported_families` returns the families supported by \code{\link{fit_model}}.
#' `supported_optimizers` returns the optimizers supported by \code{\link{fit_model}}.
#' `suported_plots` returns the plots supported by argument `model`.
#' @rdname model_info
#' @export
supported_models <- function() {
  c(
    "HDI2020", "HD2022", "RW1972", "MAC1975",
    "PKH1982", "SM2007", "ANCCR", "RAND"
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
    "RW1972" = c("es", "vs"),
    "MAC1975" = c("as", "es", "vs"),
    "SM2007" = c("acts", "relacts", "vs", "os"),
    "PKH1982" = c("as", "es", "eivs"),
    "ANCCR" = c("anccr"),
    "RAND" = c("es", "vs")
  )
  if (is.null(model)) {
    plot_info
  } else {
    model <- .calmr_assert("supported_model", model)
    plot_info[[model]]
  }
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
