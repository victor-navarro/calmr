require(methods)
#' S4 class for  Calmr results
#'
#' @section Slots:
#' \describe{
#' \item{\code{results}:}{Data.frame or List. The unparsed model results}
#' \item{\code{parsed_results}:}{List. The parsed and aggregated model results}
#' \item{\code{is_parsed}:}{Logical. Whether the model results have been parsed}
#' }
#' @name CalmrResult
#' @rdname CalmrResult
#' @exportClass CalmrResult
#' @import tibble

methods::setClass(
  "CalmrResult",
  representation(
    results = "tbl",
    raw_results = "list"
  ),
  prototype(
    results = NULL,
    raw_results = NULL
  )
)

# Method to print results
setMethod("show", "CalmrResult", function(object) {
  if (!is.null(object@results)) {
    print(object@results)
  } else {
    print(object@raw_results)
  }
})

# TODO: Expand this class to specific
# types of results (e.g., experiments, fits, comparisons)

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
    map = "list",
    raw_design = "data.frame"
  )
)

methods::setMethod(
  "show", "CalmrDesign",
  function(object) print(object@design)
)

methods::setGeneric(
  "trials",
  function(x) methods::standardGeneric("trials")
)
methods::setMethod(
  "trials",
  "CalmrDesign", function(x) {
    trial_dat <- data.frame()
    for (r in seq_len(nrow(x))) {
      td <- x$trial_info[[r]]
      gdf <- data.frame(
        group = x$group[r], phase = x$phase[r],
        td[c("trial_names", "trial_repeats", "is_test")],
        data.frame(lapply(td[c("trial_functional")], paste))
      )
      trial_dat <- rbind(trial_dat, gdf)
    }
    return(trial_dat)
  }
)

methods::setGeneric("map", function(x) methods::standardGeneric("map"))
methods::setMethod("map", "CalmrDesign", function(x) {
  print(x@map)
})


#' S4 class for Calmr Comparisons
#'
#' @section Slots:
#' \describe{
#' \item{\code{results}:}{Data.frame. The unparsed model results}
#' \item{\code{models}:}{Character vector. The models being compared}
#' \item{\code{layers}:}{Character vector. The layers being compared}
#' \item{\code{model_layer_names}:}{Character vector. Combination of the two above}
#' }
#' @name CalmrComparison-class
#' @rdname CalmrComparison-class
#' @exportClass CalmrComparison
setClass("CalmrComparison",
  slots = c(
    results = "data.frame",
    models = "character",
    layers = "character",
    model_layer_names = "character"
  )
)

#' S4 class for Calmr RSA
#'
#' @section Slots:
#' \describe{
#' \item{\code{corr_mat}:}{Array. Correlation matrix}
#' \item{\code{distance_mats}:}{List. Distance matrices}
#' \item{\code{trials}:}{Character. Trial option used to calculate distance mats}
#' \item{\code{dist_method}:}{Character vector. Method to calculate distances}
#' \item{\code{corr_method}:}{Character vector. Method to calculate correlations}
#' }
#' @name CalmrRSA-class
#' @rdname CalmrRSA-class
#' @exportClass CalmrRSA
setClass("CalmrRSA",
  slots = c(
    corr_mat = "array",
    distance_mats = "list",
    trials = "character",
    dist_method = "character",
    corr_method = "character"
  )
)
#' S4 class for Calmr RSA Test
#'
#' @section Slots:
#' \describe{
#' \item{\code{RSA}:}{Object of class CalmrRSA}
#' \item{\code{sig_mat}:}{Array. Correlation significance matrix}
#' \item{\code{lower_crit}:}{Array. Lower bound for significance}
#' \item{\code{upper_crit}:}{Array. Upper bound for significance}
#' \item{\code{n_samples}:}{Numeric. Number of bootstrap samples}
#' \item{\code{p}:}{Numeric. Alpha level for significance}
#' }
#' @name CalmrRSATest-class
#' @rdname CalmrRSATest-class
#' @exportClass CalmrRSATest
setClass("CalmrRSATest",
  slots = c(
    RSA = "CalmrRSA",
    sig_mat = "array",
    lower_crit = "array",
    upper_crit = "array",
    n_samples = "numeric",
    p = "numeric"
  )
)

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

#### DEPRECATE ####
#' S4 class for Calmr Models
#'
#' @section Slots:
#' \describe{
#' \item{\code{model}:}{Character. The model name.}
#' \item{\code{parameters}:}{List. The model parameters.}
#' \item{\code{model_results }:}{List. The model results (layers)}
#' \item{\code{experience}:}{Data.frame. The data.frame used to train the model.}
#' \item{\code{mapping}:}{List. Contains mapping information (e.g., between functional and nominal stimuli)}
#' \item{\code{is_parsed}:}{Logical. Whether the model results have been parsed.}
#' }
#' @name CalmrModel-class
#' @rdname CalmrModel-class
#' @exportClass CalmrModel

setClass("CalmrModel",
  slots = c(
    model = "character",
    parameters = "list",
    model_results = "list",
    experience = "data.frame",
    mapping = "list",
    is_parsed = "logical"
  ),
  prototype = list(is_parsed = FALSE)
)
