#' @title Run models given a set of parameters
#' @param design A data.frame of dimensions G,2*P+1; where G is the number of groups and P is the number of phases.
#' @param models A character vector specifying the models to run.
#' @param comparison A character vector of length M (the length of `models`), specifying the layer in each model that will be used for comparison.
#' @param params A list of length M, each entry containing a data.frame of dimensions N,2; where N is the number of stimuli in the experimental design.
#' @param options A list of options, as returned by get_model_opts.
#' @return A list with parsed results or a tibble with raw results
#' TODO: make the binding of models more robust
#' @export


compare_models <- function(design, models, comparison, params = NULL, options = get_model_opts()){
  supported_models = get_supported_models()
  if (any(!(models %in% supported_models))) stop("One or more of the models provided are not supported.")

  #parse design if not parsed
  if (!tibble::is_tibble(design)){
    design = parse_design(design)
  }

  if (is.null(params)){
    cat("Warning: Parameters not provided. Using default parameters for all models\n")
    params = sapply(models, function(m) get_params(design, model = m))
  }


  #make the arguments for all the models
  args = sapply(1:length(models),
                function(m){
                  make_model_args(design = design, pars = params[[m]], model = models[m], opts = options)
                })

  #now overwrite experience and mapping so all the models have the same
  args = lapply(args, function(x) {
    x['experience'] = args[[1]]['experience']
    x['mapping'] = args[[1]]['mapping']
    x
  })

  #run models
  exps = lapply(args, function(x) run_model(x))

  #filter comparison layers and put them together
  res = do.call("rbind",
                lapply(1:length(models), function(x){
                  h = exps[[x]]@parsed_results[[comparison[x]]]
                  h$model = models[x]
                  h
                }))

  new("HeidiComparison",
      results = res,
      raw_results = exps)
}
