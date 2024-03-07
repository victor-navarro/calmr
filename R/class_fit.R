#' S4 class for Calmr Fit
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

#' CalmrFit Methods
#' @param object A CalmrFit
#' @rdname CalmrFit-methods
#' @export
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

#' @param object A CalmrFit
#' @param type A string specifying the type of prediction to generate
#' @rdname CalmrFit-methods
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
setGeneric("NLL", function(object, ...) standardGeneric("NLL"))
#' @param object A CalmrFit
#' @rdname CalmrFit-methods
#' @export
NLL <- function(object) NULL # nolint: object_name_linter.
setMethod("NLL", "CalmrFit", function(object) {
  object@nloglik
})

#' @param object A CalmrFit
#' @param k Penalty term
#' @details The AIC is defined as `2*k - 2*-NLL`, where k is a penalty
#' term and NLL is the negative log likelihood of the model.
#' @rdname CalmrFit-methods
#' @export
setMethod(
  "AIC", "CalmrFit",
  function(object, k = 2) {
    k * length(object@best_pars) - 2 * -object@nloglik
  }
)

#' @param object A CalmrFit
#' @details The BIC is defined as `k*log(n) - 2*-NLL`, where k is the number
#' of parameters in the model and n is the number of observations
#' @rdname CalmrFit-methods
#' @export
setMethod(
  "BIC", "CalmrFit",
  function(object) {
    length(object@best_pars) * log(length(object@data)) -
      2 * -object@nloglik
  }
)
