#' S4 class for calmr Models
#'
#' @section Slots:
#' \describe{
#' \item{model}{A model name string}
#' \item{model_fn}{The model function}
#' \item{state}{A list with the model internal state}
#' \item{parameters}{A list with the model with model parameters}
#' }
#' @name CalmrModel-class
#' @rdname CalmrModel-class
#' @note Currently unused.
#' @noRd

setClass(
  "CalmrModel",
  representation(
    model = "character",
    model_fn = "function",
    state = "list",
    parameters = "list"
  )
)

# Future note: This class will have its own "forward" method that takes
# a mapping and experience parameter and returns result.
# That way one can save the state of the model and retrain it. In fact,
# that might be a more elegant way of dealing with phases.
# Given that the "forward" method will return a list, the current
# implementation does not conflict with future changes.
