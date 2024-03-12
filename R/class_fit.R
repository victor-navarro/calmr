#' S4 class for calmr Fit
#'
#' @section Slots:
#' \describe{
#' \item{\code{nloglik}:}{Numeric. Negative log likelihood of the fit}
#' \item{\code{best_pars}:}{Numeric. Best fitting parameters}
#' \item{\code{model_pars}:}{Numeric. Parameters used in the model function}
#' \item{\code{link_pars}:}{Numeric. Parameters used in the link function}
#' \item{\code{data}:}{Numeric. Data used for fit}
#' \item{\code{model_function}:}{Function. Model function}
#' \item{\code{link_function}:}{Function. Link function}
#' \item{\code{ll_function}:}{Function. Objective function
#' (usually nloglikelihood)}
#' \item{\code{optimizer_options}:}{List. Options used for the optimizer}
#' \item{\code{extra_pars}:}{List. Extra parameters
#' passed to the fit call (...)}
#' }
#' @exportClass CalmrFit
#' @seealso CalmrFit-methods
setClass("CalmrFit",
  slots = c(
    nloglik = "numeric",
    best_pars = "numeric",
    model_pars = "numeric",
    link_pars = "numeric",
    data = "numeric",
    model_function = "function",
    link_function = "function",
    ll_function = "function",
    optimizer_options = "list",
    extra_pars = "list"
  )
)

#' CalmrFit methods
#' @description S4 methods for `CalmrFit` class.
#' @param object A `CalmrFit` object.
#' @param k Penalty term for `AIC` method.
#' @param type A string specifying the type of prediction to generate.
#' @param ... Extra named arguments.
#' @name CalmrFit-methods
#' @details
#' With `type = "response"`, the `predict()` function
#' passed model responses to the link function used to fit the model.
#'
#' The AIC is defined as `2*k - 2*-NLL`, where k is a penalty
#' term and NLL is the negative log likelihood of the model.
#'
#' The BIC is defined as `p*log(n) - 2*-NLL`, where p is the number
#' of parameters in the model and n is the number of observations
#' @returns
#' * `show()` returns NULL (invisibly).
#' * `predict()` returns a numeric vector.
#' * `NLL()` returns the negative log likelihood of the model.
#' * `AIC()` returns the Akaike Information Criterion (AIC) of the model.
#' * `BIC()` returns the Bayesian Information Criterion (BIC) of the model.
NULL
#> NULL



#' @rdname CalmrFit-methods
#' @export
setMethod("show", "CalmrFit", function(object) {
  message(
    "Calmr model fit\n",
    "--------------\n",
    "Parameters:\n",
    paste0(utils::capture.output(object@best_pars), collapse = "\n"),
    "\n",
    "--------------\n",
    "\nnLogLik: ", round(object@nloglik, 4)
  )
})

#' @rdname CalmrFit-methods
#' @aliases predict
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

setGeneric("NLL", function(object) standardGeneric("NLL"))
#' @rdname CalmrFit-methods
#' @aliases NLL
#' @export
setMethod("NLL", "CalmrFit", function(object) {
  object@nloglik
})

#' @rdname CalmrFit-methods
#' @aliases AIC
#' @export
setMethod(
  "AIC", "CalmrFit",
  function(object, k = 2) {
    k * length(object@best_pars) - 2 * -object@nloglik
  }
)

#' @rdname CalmrFit-methods
#' @aliases BIC
#' @export
setMethod(
  "BIC", "CalmrFit",
  function(object) {
    length(object@best_pars) * log(length(object@data)) -
      2 * -object@nloglik
  }
)
