#' S4 class for Calm designs
#'
#' @description Inherits from the tbl class
#' @section Slots:
#' \describe{
#' \item{\code{design}:}{A tbl containing the object.}
#' \item{\code{mapping}:}{A list containing the object mapping.}
#' \item{\code{raw_design}:}{The original data.frame.}
#' \item{\code{augmented}:}{Whether the object has been augmented.}
#' }
#' @exportClass CalmDesign
methods::setClass(
  "CalmDesign",
  representation(
    design = "tbl",
    mapping = "list",
    raw_design = "data.frame",
    augmented = "logical"
  ),
  prototype(augmented = FALSE)
)
#' CalmDesign methods
#' @export
#' @rdname CalmDesign-methods
methods::setMethod(
  "show", "CalmDesign",
  function(object) print(object@design)
)

#' @rdname CalmDesign-methods
methods::setGeneric(
  "mapping",
  function(object) methods::standardGeneric("mapping")
)
#' CalmDesign methods
#' @description Methods mapping, and trials, are extractor functions.
#' @param object A CalmDesign, as returned by parse_design
#' @export
#' @rdname CalmDesign-methods
methods::setMethod(
  "mapping", "CalmDesign",
  function(object) object@mapping
)
#' @export
#' @rdname CalmDesign-methods
methods::setGeneric(
  "trials",
  function(object) methods::standardGeneric("trials")
)
#' @export
#' @rdname CalmDesign-methods
methods::setMethod(
  "trials", "CalmDesign",
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
#' Augment CalmDesign
#' @param object A CalmDesign, as returned by parse_design
#' @param model A modelname string. One of supported_models()
#' @param ... Additional parameters depending on the model.
#' @rdname CalmDesign-methods
#' @aliases augment
#' @order 1
#' @export
methods::setMethod("augment", "CalmDesign", function(object, model, ...) {
  if (model %in% c("ANCCR")) {
    # creates eventlogs
    object <- .anccrize_design(object, ...)
    object@augmented <- TRUE
  }
  object
})
