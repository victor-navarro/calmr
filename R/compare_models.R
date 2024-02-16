#' @title Run models given a set of parameters
#' @param design A design data.frame or CalmrDesign
#' @param models A character vector of length m, specifying the models to run.
#' @param parameters A list of length m,
#' with each entry containing a list with model parameters,
#' as returned by `get_parameters`
#' @param options A list of options, as returned by get_exp_opts.
#' @returns A CalmrExperiment object
#' @export
#' @examples
#' exp <- data.frame(
#'   Group = c("A", "B"),
#'   P1 = c("10(A)>(US)/5B>(US)", "5(A)>(US)/10B>(US)"),
#'   R1 = TRUE
#' )
#' exp <- parse_design(exp)
#' models <- c("HD2022", "RW1972", "PKH1982")
#' parameters <- sapply(models, get_parameters, design = exp)
#' comp <- compare_models(exp, models = models, parameters = parameters)
compare_models <- function(
    design, models,
    parameters = NULL,
    options = NULL) {
  supported_models <- supported_models()

  # assert models
  models <- sapply(models, .calmr_assert, what = "supported_model")

  if (is.null(parameters)) {
    warning("Parameters not provided.
    Using default parameters for all models.\n")
    parameters <- sapply(models, function(m) {
      get_parameters(design, model = m)
    }, simplify = FALSE)
  } else {
    # assert length parameters
    .calmr_assert("length", length(models),
      models = models, parameters = parameters
    )
    # assert parameters
    parameters <- sapply(seq_along(models), function(i) {
      .calmr_assert("parameters",
        parameters[[i]],
        design = design, model = models[i]
      )
    })
  }

  # assert options
  options <- .calmr_assert("experiment_options", options)

  # make the arguments for all the models
  args <- sapply(seq_len(length((models))),
    function(m) {
      make_experiment(
        design = design, parameters = parameters[[m]],
        model = models[m], options = options
      )
    },
    simplify = FALSE, USE.NAMES = FALSE
  )

  # now overwrite experience and mapping
  # so all the models iterations have the same experience
  args <- lapply(args, function(x) {
    x@arguments$experience <- args[[1]]@arguments$experience
    x@arguments$mapping <- args[[1]]@arguments$mapping
    x
  })

  # run experiments
  do.call(c, lapply(args, function(x) run_experiment(x)))
}
