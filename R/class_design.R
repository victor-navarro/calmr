#' S4 class for Calmr designs
#'
#' @description Inherits from the tbl class
#' @section Methods:
#' \describe{
#' \item{\code{trials}:}{Prints trial information per group and phase}
#' }
#' @name CalmrDesign
#' @rdname CalmrDesign
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
methods::setMethod(
  "show", "CalmrDesign",
  function(object) print(object@design)
)


methods::setGeneric("mapping", function(x) {
  methods::standardGeneric("mapping")
})
methods::setMethod(
  "mapping", "CalmrDesign",
  function(x) x@mapping
)

methods::setGeneric(
  "trials",
  function(x) methods::standardGeneric("trials")
)
methods::setMethod(
  "trials", "CalmrDesign",
  function(x) {
    des <- x@design
    trial_dat <- data.frame()
    for (r in seq_len(nrow(des))) {
      td <- des$phase_info[[r]]
      gdf <- data.frame(
        group = des$group[r],
        phase = des$phase[r],
        td$general_info[c("trial_names", "trial_repeats", "is_test")]
      )
      gdf$stimuli <- lapply(
        x@mapping$trial_nominals[gdf$trial_names],
        function(x) paste(x, collapse = ";")
      )
      trial_dat <- rbind(trial_dat, gdf)
    }
    return(trial_dat)
  }
)
