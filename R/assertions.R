is_experiment <- function(object) {
  inherits(object, "CalmrExperiment")
}

is_design <- function(object) {
  inherits(object, "CalmrDesign")
}

#' Sanitize model outputs
#' @param os Given outputs. Character vector
#' @param m A model name
#' @return A character vector
#' @note If os is not NULL, cuts extraneous outputs.
#' @noRd
.sanitize_outputs <- function(os, m) {
  moutputs <- model_outputs(m)
  if (is.null(os)) {
    return(moutputs)
  }
  # extra parameters
  extra <- setdiff(os, moutputs)
  throw_warn <- length(extra) > 0
  os <- os[!(os %in% extra)]
  # final parameters
  if (!length(os)) {
    stop("Ended with zero outputs after sanitization. Check your outputs.")
  }
  if (throw_warn) warning("Found unsupported outputs. Trimming...")
  os
}

.assert_model <- function(model) {
  stopifnot(
    "Model must be one returned by `supported_models()`" =
      (model %in% supported_models())
  )
  model
}

.assert_timed_model <- function(model) {
  stopifnot(
    "Model must be one returned by `supported_timed_models()`" =
      (model %in% supported_timed_models())
  )
  model
}

.assert_parsed_design <- function(design) {
  if ("CalmrDesign" %in% class(design)) {
    return(design)
  }
  parse_design(design)
}

.assert_single_model <- function(model) {
  stopifnot(
    "Model must be length 1" =
      length(model) == 1
  )
}

.assert_timings <- function(timings, design, model) {
  def_timings <- get_timings(design, model)
  if (!is.null(timings)) {
    stopifnot(
      "Timing lists must be equally named" =
        .check_deep_lists(timings, def_timings)
    )
    return(timings)
  }
  warning("Using default design timings.")
  def_timings
}

.assert_parameters <- function(parameters, model, design) {
  def_parameters <- get_parameters(design, model = model)
  if (!is.null(parameters)) {
    stopifnot(
      "Parameter lists must be equally named" =
        .check_deep_lists(parameters, def_parameters)
    )
    return(parameters)
  }
  warning("Using default model parameters.")
  def_parameters
}

# returns true if two lists have the same names and length
.check_deep_lists <- function(a, b) {
  anames <- sort(names(unlist(a)))
  bnames <- sort(names(unlist(b)))
  (length(anames) == length(bnames)) &&
    all(anames == bnames)
}

.assert_optimizer <- function(optimizer) {
  if (is.null(optimizer)) {
    warning("No optimizer passed. Using 'optim'.")
    optimizer <- "optim"
  }
  stopifnot(
    "Optimizer must be one returned by supported_optimizers()" =
      (optimizer %in% supported_optimizers())
  )
  optimizer
}

.assert_family <- function(family) {
  if (is.null(family)) {
    warning("No family passed. Using 'identity'.")
    family <- "identity"
  }
  stopifnot(
    "Family must be one returned by `supported_families()`" =
      (family %in% supported_families())
  )
  family
}

.assert_limits <- function(options) {
  stopifnot(
    "You must provide upper and lower limits for parameters" =
      all(!is.na(c(options$ll, options$ul)))
  )
}

.assert_no_functional <- function(mapping) {
  stopifnot(
    "The model does not support functional/nominal stimuli specifications." =
      length(
        mapping$unique_nominal_stimuli
      ) == length(
        mapping$unique_functional_stimuli
      )
  )
}

.assert_filepath <- function(file) {
  if (!file.exists(dirname(file))) {
    stop(sprintf("Path to file %s does not exist.", file))
  }
}

.assert_experiment <- function(experiment) {
  stopifnot(
    "Experiment is too simple to run for one or more groups." =
      length(experiment@design@mapping$unique_nominal_stimuli) > 1
  )
}

.assert_valid_palette <- function(palette) {
  if (is.null(palette)) {
    return("viridis")
  }
  stopifnot(
    "Palette must be one returned by `set_calmr_palette()`" =
      palette %in% set_calmr_palette()
  )
  palette
}
