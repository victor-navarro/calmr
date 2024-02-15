#' @title Run models given a set of parameters
#' @param design A design data.frame
#' @param models A character vector specifying the models to run.
#' @param layers A character vector of length M (the length of `models`),
#' specifying the layers to compare
#' specifying the layer in each model that will be used for layers.
#' @param parameters A list of length M,
#' each containing a list with model parameters,
#' as returned by `get_parameters``
#' @param options A list of options, as returned by get_exp_opts.
#' @returns A CalmrComparison object.f
#' @export


compare_models <- function(
    design, models, layers, parameters = NULL,
    options = get_exp_opts()) {
  supported_models <- supported_models()
  if (any(!(models %in% supported_models))) {
    stop("One or more of the models provided are not supported.")
  }

  if (is.null(parameters)) {
    warning("Parameters not provided.
    Using default parameters for all models.\n")
    parameters <- sapply(models, function(m) {
      get_parameters(design, model = m)
    }, simplify = FALSE)
  }

  # make the arguments for all the models
  args <- sapply(seq_len(length((models))),
    function(m) {
      make_experiment(
        design = design, parameters = parameters[[m]],
        model = models[m], options = options
      )
    },
    simplify = FALSE
  )

  # now overwrite experience and mapping
  # so all the models iterations have the same experience
  args <- lapply(args, function(x) {
    x@arguments$experience <- args[[1]]@arguments$experience
    x@arguments$mapping <- args[[1]]@arguments$mapping
    x
  })

  # run experiments
  exps <- do.call(c, lapply(args, function(x) run_experiment(x)))

  return(exps)

  methods::new("CalmrComparison",
    results = res,
    models = models,
    layers = layers,
    model_layer_names = sprintf("%s (%s)", models, layers)
  )
}
