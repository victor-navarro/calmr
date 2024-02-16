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
#' \item{\code{ll_function}:}{Function. Objective function (usually nloglikelihood)}
#' \item{\code{optimizer_options}:}{List. Options used for the optimizer}
#' \item{\code{extra_pars}:}{List. Extra parameters passed to the fit call (...)}
#' }
#' @name CalmrFit-class
#' @rdname CalmrFit-class
#' @exportClass CalmrFit
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
