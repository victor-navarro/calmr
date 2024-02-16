require(methods)

#### Generics ####
setGeneric("graph", function(x, ...) standardGeneric("graph"))
setGeneric("NLL", function(object, ...) standardGeneric("NLL"))
setGeneric("get_output", function(object, ...) standardGeneric("get_output"))

#### Methods ####

#### Exposing methods ####
setMethod("show", "CalmrFit", function(object) {
  cat(
    "Calmr model fit: \n ",
    "Parameters: \n"
  )
  print(object@best_pars)
  cat(
    "\n",
    "nLogLik: \n", object@nloglik, "\n"
  )
})




# setMethod(
#   "plot", "CalmrModel",
#   function(x, type = NULL, ...) {
#     if (is.null(type)) {
#       type <- "vs"
#     }
#     .calmr_assert("supported_plot", type,
#       supported = names(x@parsed_results)
#     )
#     plotf <- get(paste0("plot_", type))

#     if (x@is_parsed) {
#       plotf(x@model_results[[type]], ...)
#     } else {
#       plotf(.parse(x, type), ...)
#     }
#   }
# )


# setMethod("plot", "CalmrComparison", function(x, ...){
#   #try to make the matrix wide
#   dat = x@results %>% tidyr::pivot_wider(names_from = "model", values_from = "value")
#   if (any(is.na(dat))){
#     warning("Some data entries are not shared among all models and will be omitted.")
#     dat = stats::na.omit(dat)
#   }
#
#   GGally::ggpairs(dat, columns = x@model_layer_names,
#                   ggplot2::aes(colour = group),
#                   diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)))
#
# })




#### Predict methods ####
#' Predict from CalmrFit
#'
#' Obtain a prediction from CalmrFit
#'
#' @param object An object of class \code{\link{CalmrFit-class}}.
#' @param type A string. If `response`, model responses are transformed via the link function.
#' @param ... Additional parameters passed to the function (`object@model_function`).
#' @export
setMethod(
  "predict", "CalmrFit",
  function(object, type = "response", ...) {
    prediction <- object@model_function(object@model_pars, ...)
    if (type == "response") {
      prediction <- object@link_function(prediction, object@link_pars)
    }
    prediction
  }
)

#### GOF methods ####
#' NLL of CalmrFit
#'
#' Returns the negative log likelihood of a model fit via calmr::fit_model
#'
#' @param object An object of class \code{\link{CalmrFit-class}}.
#' @param ... Unused
#' @return A numeric
#' @export
NLL <- function(object, ...) NULL
setMethod("NLL", "CalmrFit", function(object) {
  object@nloglik
})

#' AIC of CalmrFit
#'
#' Returns the Akaike Information Criterion of a model fit via calmr::fit_model
#'
#' @param object An object of class \code{\link{CalmrFit-class}}.
#' @param ... Unused
#' @param k Numeric. Penalty term (default = 2)
#' @return A numeric
#' @details The AIC is defined as `2*k - 2*-NLL`, where k is a penalty term and NLL is the negative log likelihood of the model.
#' @export
setMethod(
  "AIC", "CalmrFit",
  function(object, ..., k = 2) {
    k * length(object@best_pars) - 2 * -object@nloglik
  }
)
#' BIC of CalmrFit
#'
#' Returns the Bayesian Information Criterion of a model fit via calmr::fit_model
#'
#' @param object An object of class \code{\link{CalmrFit-class}}.
#' @param ... Unused
#' @return A numeric
#' @details The AIC is defined as `k*log(n) - 2*-NLL`, where k is the number of parameters in the model and n is the number of observations
#' @export
setMethod(
  "BIC", "CalmrFit",
  function(object, ...) {
    length(object@best_pars) * log(length(object@data)) -
      2 * -object@nloglik
  }
)


#### Comparison methods ####
