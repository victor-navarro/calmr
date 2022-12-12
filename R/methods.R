#### Generics ####
setGeneric("graph", function(x,...) standardGeneric("graph"))
setGeneric("calmr_plot", function(x, type, ...) standardGeneric("calmr_plot"))

#### Exposing methods ####

setMethod("show", "CalmrModel", function(object){
  cat(object@model, "run with:\n\n")
  print(object@parameters)
  print(graph(object, colour_key = T))
})
setMethod("show", "CalmrExperiment", function(object){
  print(object@results)
})
setMethod("show", "CalmrComparison", function(object){
  if (object@is_parsed){
    print(object@parsed_results)
  }else{
    print(object@results)
  }
})

setMethod("show", "CalmrRSA", function(object){
  cat("Representational Similarity Analysis\n\n",
      sprintf("Distance metric: %s\n", object@dist_method),
      sprintf("Correlation method: %s\n\n", object@corr_method),
      "Correlation matrix:\n\n")
  print(object@corr_mat)
})

setMethod("show", "CalmrRSATest", function(object){
  show(object@RSA)
  cat("\nSignificance matrix:\n\n")
  print(object@sig_mat)
  cat(sprintf("\n%d permutation samples, two-tailed test with alpha = %1.2f.\n",
              object@n_samples, 1-object@p))
})


#### Ploting methods ####
#' @export
calmr_plot <- function(x, ...) NULL

#' @export
calmr_graph <- function(x, ...) NULL

setMethod("calmr_plot", "CalmrExperiment",
          function(x, type = NULL, ...){
            if (is.null(type)){ type = "vs"}
            if (!x@is_parsed){ x = parse_experiment_results(x)} #parse if model has not been parsed
            if (!(type %in% names(x@parsed_results))) stop(sprintf("Model does not contain '%s' in model results.", type))
            plotf = switch(type,
                           "vs" = plot_vs,
                           "es" = plot_vs,
                           "as" = plot_as,
                           "acts" = plot_acts,
                           "rs" = plot_rs)
            dat = x@parsed_results[[type]]
            groups = unique(dat$group)
            ps = sapply(groups, function(g) plotf(dat[dat$group == g,], ...) + ggplot2::labs(title = sprintf("Group = %s", g)), simplify = F)
            ps
          })

setMethod("calmr_plot", "CalmrModel",
          function(x, type = NULL, ...){
            if (is.null(type)){ type = "vs"}
            if (!(type %in% names(x@model_results))) stop(sprintf("Model does not contain '%s' in model results.", type))
            plotf = switch(type,
                           "vs" = plot_vs,
                           "es" = plot_vs,
                           "as" = plot_as,
                           "acts" = plot_acts,
                           "rs" = plot_rs)
            if (x@is_parsed){
              plotf(x@model_results[[type]], ...)
            }else{
              plotf(.parse(x, type), ...)
            }
          })


setMethod("calmr_plot", "CalmrComparison", function(x, ...){
  #try to make the matrix wide
  dat = x@results %>% tidyr::pivot_wider(names_from = "model", values_from = "value")
  if (any(is.na(dat))){
    warning("Some data entries are not shared among all models and will be omitted.")
    dat = stats::na.omit(dat)
  }

  GGally::ggpairs(dat, columns = x@model_layer_names,
                  ggplot2::aes(colour = group),
                  diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)))

})

setMethod("calmr_plot", "CalmrRSA",
          function(x, ...){
            corrmat = x@corr_mat
            corrmat[lower.tri(corrmat)] = NA
            dat = data.frame(as.table(corrmat))
            dat %>% ggplot2::ggplot(ggplot2::aes(x = Var1, y = Var2, fill = Freq, label = round(Freq, 2))) +
              ggplot2::geom_tile(na.rm = T) +
              ggplot2::geom_text(na.rm = T) +
              ggplot2::scale_fill_gradient2(limits = c(-1, 1), na.value = "white") +
              ggplot2::theme(axis.title = ggplot2::element_blank(),
                             panel.background = ggplot2::element_blank()) +
              ggplot2::labs(fill = "Correlation") +
              ggplot2::scale_x_discrete(position = "top")
          })

setMethod("calmr_plot", "CalmrRSATest",
          function(x, ...){
            p = calmr_plot(x@RSA)
            sigmat = x@sig_mat
            sigmat[lower.tri(sigmat)] = NA
            dat = p$data
            dat$sig = data.frame(as.table(sigmat))$Freq
            p + ggplot2::geom_label(data = na.omit(dat[dat$sig,]), fill = "white")
          })

setMethod("calmr_graph", "CalmrModel", function(x, ...){
  if (x@is_parsed){
    graph_weights(x@model_results$vs, ...)
  }else{
    graph_weights(.parse(x, "vs"), ...)
  }
})

setMethod("calmr_graph", "CalmrExperiment", function(x, ...){
  dat = x@parsed_results$vs
  groups = unique(dat$group)
  ps = sapply(groups, function(g) graph_weights(dat[dat$group == g,], ...) +
                ggplot2::labs(title = sprintf("Group = %s", g)), simplify = F)
  ps
})

