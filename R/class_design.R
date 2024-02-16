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

methods::setClass("CalmrDesign",
  slots = c(
    design = "tbl",
    mapping = "list",
    raw_design = "data.frame"
  )
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


methods::setGeneric("trials", function(x) {
  methods::standardGeneric("trials")
})
methods::setMethod(
  "trials", "CalmrDesign",
  function(x) {
    des <- x@design
    trial_dat <- data.frame()
    for (r in seq_len(nrow(des))) {
      td <- des$trial_info[[r]]
      gdf <- data.frame(
        group = des$group[r], phase = des$phase[r],
        td[c("trial_names", "trial_repeats", "is_test")],
        data.frame(lapply(td[c("trial_functional")], paste))
      )
      trial_dat <- rbind(trial_dat, gdf)
    }
    return(trial_dat)
  }
)
