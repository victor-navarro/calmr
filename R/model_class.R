#' S4 class for Calmr Models
#'
#' @section Slots:
#' \describe{
#' \item{model}{A model name string}
#' \item{model_fn}{The model function}
#' \item{state}{A list with the model internal state}
#' }
#' @name CalmrModel-class
#' @rdname CalmrModel-class
#' @note Currently unused.
#' @exportClass CalmrModel

setClass(
  "CalmrModel",
  representation(
    model = "character",
    model_fn = "function",
    state = "list"
  )
)
