#' Get timing design parameters
#' @param design A `data.frame` containing the experimental design.
#' @return A list of timing design parameters.
#' @export
#' @examples
#' block <- get_design("blocking")
#' get_timings(block)
get_timings <- function(design) {
  parsed_design <- .assert_parsed_design(design)
  # Get trial names from design
  trialnames <- mapping(parsed_design)$trial_names
  # Get transition names from design
  transitions <- mapping(parsed_design)$transitions

  trial_pars <- do.call(
    .named_pars,
    c(.default_trial_timings(), list(trialnames))
  )

  trans_pars <- list()
  if (length(transitions)) {
    def_trans <- .default_transition_timings()
    trans_pars <- mapply(
      function(n, v) {
        sapply(transitions, function(trans) {
          stats::setNames(rep(v, length(trans)), trans)
        }, simplify = FALSE)
      },
      n = def_trans$name,
      v = def_trans$default_value,
      SIMPLIFY = FALSE
    )
  }

  c(.default_global_timings(), trial_pars, trans_pars)
}

# Default timing parameter information
.default_trial_timings <- function() {
  list(name = c(
    "post_trial_delay",
    "mean_ITI", "max_ITI"
  ), default_value = c(1, 30, 90, 1))
}
.default_transition_timings <- function() {
  list(name = c(
    "transition_delay"
  ), default_value = c(1))
}
.default_global_timings <- function() {
  list("jitter" = 0.1, "use_exponential" = TRUE)
}

# Returns whether a parameter is a trial parameter
.is_trial_parameter <- function(parameter) {
  trial_pars <- list(
    "post_trial_delay",
    "mean_ITI", "max_ITI"
  )
  parameter %in% trial_pars
}

# Returns wheter a parameter is a transition parameter
.is_trans_parameter <- function(parameter) {
  trans_pars <- list(
    "transition_delay"
  )
  parameter %in% trans_pars
}
