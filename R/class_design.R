#' S4 class for Calmr designs
#'
#' @description Inherits from the tbl class
#' @section Slots:
#' \describe{
#' \item{\code{design}:}{A tbl containing the object.}
#' \item{\code{mapping}:}{A list containing the object mapping.}
#' \item{\code{raw_design}:}{The original data.frame.}
#' \item{\code{augmented}:}{Whether the object has been augmented.}
#' }
#' @exportClass CalmrDesign
methods::setClass(
  "CalmrDesign",
  representation(
    design = "tbl",
    mapping = "list",
    raw_design = "data.frame",
    augmented = "logical"
  ),
  prototype(augmented = FALSE)
)
#' CalmrDesign methods
#' @export
#' @rdname CalmrDesign-methods
methods::setMethod(
  "show", "CalmrDesign",
  function(object) print(object@design)
)

methods::setGeneric(
  "mapping",
  function(object) methods::standardGeneric("mapping")
)
#' CalmrDesign methods
#' @description Methods mapping, and trials, are extractor functions.
#' @param object A CalmrDesign, as returned by parse_design
#' @export
#' @rdname CalmrDesign-methods
methods::setMethod(
  "mapping", "CalmrDesign",
  function(object) object@mapping
)
#' @export
#' @rdname CalmrDesign-methods
methods::setGeneric(
  "trials",
  function(object) methods::standardGeneric("trials")
)
#' @export
#' @rdname CalmrDesign-methods
methods::setMethod(
  "trials", "CalmrDesign",
  function(object) {
    des <- object@design
    trial_dat <- data.frame()
    for (r in seq_len(nrow(des))) {
      td <- des$phase_info[[r]]
      gdf <- data.frame(
        group = des$group[r],
        phase = des$phase[r],
        td$general_info[c("trial_names", "trial_repeats", "is_test")]
      )
      gdf$stimuli <- lapply(
        object@mapping$trial_nominals[gdf$trial_names],
        function(object) paste(object, collapse = ";")
      )
      trial_dat <- rbind(trial_dat, gdf)
    }
    return(trial_dat)
  }
)

methods::setGeneric(
  "augment",
  function(object, model, ...) methods::standardGeneric("augment")
)
#' Augment CalmrDesign
#' @param object A CalmrDesign, as returned by parse_design
#' @param model A modelname string. One of supported_models()
#' @param ... Additional parameters depending on the model.
#' @rdname CalmrDesign-methods
#' @aliases augment
#' @order 1
#' @export
methods::setMethod("augment", "CalmrDesign", function(object, model, ...) {
  if (model %in% c("ANCCR")) {
    # creates eventlogs
    object <- .anccrize_design(object, ...)
    object@augmented <- TRUE
  }
  object
})
