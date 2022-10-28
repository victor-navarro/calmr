setClass("HeidiModel",
         slots = c(model = "character",
                   parameters = "list",
                   model_results = "list",
                   experience = "data.frame",
                   mapping = "list",
                   is_parsed = "logical"),
         prototype = list(is_parsed = F))

setMethod("show", "HeidiModel", function(object){
  cat(object@model, "run with:\n\n")
  print(object@parameters)
  print(graph(object, colour_key = T))
})

setGeneric("vs", function(x) standardGeneric("vs"))
setMethod("vs", "HeidiModel", function(x) .parse(x, "vs"))

setGeneric("plot", function(x, type = "vs", ...) standardGeneric("plot"))
setMethod("plot", "HeidiModel", function(x, type = "vs", ...){
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


