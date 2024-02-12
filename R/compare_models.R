#' @title Run models given a set of parameters
#' @param design A data.frame of dimensions G,2*P+1; where G is the number of groups and P is the number of phases.
#' @param models A character vector specifying the models to run.
#' @param layers A character vector of length M (the length of `models`), specifying the layer in each model that will be used for layers.
#' @param params A list of length M, each entry containing a data.frame of dimensions N,2; where N is the number of stimuli in the experimental design.
#' @param options A list of options, as returned by get_exp_opts.
#' @returns A CalmrComparison object.
#' @export
compare_models <- function(design, models, layers, params = NULL, options = get_exp_opts()) {
  supported_models <- supported_models()
  if (any(!(models %in% supported_models))) stop("One or more of the models provided are not supported.")

  parsed_design <- parse_design(design)

  if (is.null(params)) {
    warning("Parameters not provided. Using default parameters for all models.\n")
    params <- sapply(models, function(m) get_parameters(design, model = m), simplify = F)
  }

  # make the arguments for all the models
  args <- sapply(1:length(models),
    function(m) {
      make_model_args(design = design, pars = params[[m]], model = models[m], opts = options)
    },
    simplify = F
  )

  # now overwrite experience and mapping so all the models iterations have the same experience
  args <- lapply(args, function(x) {
    x["experience"] <- args[[1]]["experience"]
    x["mapping"] <- args[[1]]["mapping"]
    x
  })

  # run models
  exps <- lapply(args, function(x) run_model(x))

  # filter layers and put the models together
  tryCatch(
    {
      res <- do.call(
        "rbind",
        lapply(1:length(models), function(x) {
          h <- exps[[x]]@parsed_results[[layers[x]]]
          h$model <- sprintf("%s (%s)", models[x], layers[x])
          h
        })
      )
    },
    error = function(e) stop("ERROR: Could not concatenate models using requested layers.")
  )

  methods::new("CalmrComparison",
    results = res,
    models = models,
    layers = layers,
    model_layer_names = sprintf("%s (%s)", models, layers)
  )
}
