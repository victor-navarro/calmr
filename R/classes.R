setClass("HeidiModel",
         slots = c(model = "character",
                   parameters = "list",
                   model_results = "list",
                   experience = "data.frame",
                   mapping = "list",
                   is_parsed = "logical"),
         prototype = list(is_parsed = F))

setClass("HeidiExperiment",
         slots = c(results = "data.frame",
                   parsed_results = "list",
                   is_parsed = "logical"),
         prototype = list(is_parsed = F))

setClass("HeidiComparison",
         slots = c(results = "data.frame",
                   raw_results = "list"))

setMethod("show", "HeidiModel", function(object){
  cat(object@model, "run with:\n\n")
  print(object@parameters)
  print(graph(object, colour_key = T))
})

setMethod("show", "HeidiExperiment", function(object){
  print(object@results)
})

setMethod("show", "HeidiComparison", function(object){
  print(object@results)
})

setGeneric("vs", function(x) standardGeneric("vs"))
setMethod("vs", "HeidiModel", function(x) .parse(x, "vs"))

setGeneric("plot", function(x, type = NULL, ...) standardGeneric("plot"))
setMethod("plot", "HeidiModel", function(x, type = NULL, ...){
  if (is.null(type)){ type = "vs"}
  if (!(type %in% names(x@model_results))) stop(sprintf("Model does not contain '%s' in model results.", type))
  plotf = switch(type,
                 "vs" = plot_vs,
                 "as" = plot_as,
                 "acts" = plot_acts,
                 "rs" = plot_rs)
  if (x@is_parsed){
    plotf(x@model_results[[type]], ...)
  }else{
    plotf(.parse(x, type), ...)
  }
})
setMethod("plot", "HeidiComparison", function(x, ...){
  dat =  x@results %>% tidyr::pivot_wider(names_from = "model",
                                          values_from = "value")
  mods = names(dat)[-1:-7]
  GGally::ggpairs(dat, columns = mods,
                  ggplot2::aes(colour = group),
                  diag = list(continuous = wrap("densityDiag", alpha = 0.5)))

})

setGeneric("parse", function(x, ...) standardGeneric("parse"))
setMethod("parse", "HeidiModel", function(x, ...){
  if (x@is_parsed) stop("Model is already parsed.")
  model = parse_model(x, ...)
  model@is_parsed = T
  model
})

setGeneric("graph", function(x, ...) standardGeneric("graph"))
setMethod("graph", "HeidiModel", function(x, ...){
  if (x@is_parsed){
    graph_weights(x@model_results$vs, ...)
  }else{
    graph_weights(.parse(x, "vs"), ...)
  }
})


