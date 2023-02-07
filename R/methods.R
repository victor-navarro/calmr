require(methods)

#### Generics ####
setGeneric("graph", function(x,...) standardGeneric("graph"))
setGeneric("NLL", function(object, ...) standardGeneric("NLL"))

#### Exposing methods ####
setMethod("show", "CalmrModel", function(object){
  cat(object@model, "run with:\n\n")
  print(object@parameters)
})
setMethod("show", "CalmrExperiment", function(object){
  print(object@results)
})
setMethod("show", "CalmrFit", function(object){
  cat("Calmr model fit: \n ",
      "Parameters: \n")
  print(object@best_pars)
  cat("\n",
      "nLogLik: \n", object@nloglik, "\n")
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

#### Summary methods ####
setMethod("summary", "CalmrExperiment", function(object, ...){
  print(object@results$mod_data[[1]])
  cat("Association strengths:\n")
  for (g in 1:nrow(object@results)){
    vs = object@results$mod_data[[g]]@model_results$vs
    ntrials = dim(vs)[1]
    cat("Group = ", object@results$group[g],"\n")
    print(round(vs[ntrials, , ], 3))
    cat("\n")
  }
})

#### Ploting methods ####
setMethod("plot", "CalmrExperiment",
          function(x, type = NULL, ...){
            if (is.null(type)){ type = "vs"}
            if (!x@is_parsed){ x = parse_experiment_results(x)} #parse if model has not been parsed
            .calmr_check("supported_plot", type, names(x@parsed_results))
            plotf = get(paste0("plot_", type))

            if (type %in% c("evs", "ivs")){
              dat = rbind(data.frame(x@parsed_results[["evs"]], assoc_type = "Excitatory"),
                          data.frame(x@parsed_results[["ivs"]], assoc_type = "Inhibitory"))
            }else{
              dat = x@parsed_results[[type]]
            }
            groups = unique(dat$group)
            ps = sapply(groups, function(g) plotf(dat[dat$group == g,], ...) + ggplot2::labs(title = sprintf("Group = %s", g)), simplify = F)
            names(ps) = groups
            ps
          })

# setMethod("plot", "CalmrModel",
#           function(x, type = NULL, ...){
#             if (is.null(type)){ type = "vs"}
#             .calmr_check("supported_plot", type, names(x@model_results))
#             plotf = get(paste0("plot_", type))
#
#             if (x@is_parsed){
#               plotf(x@model_results[[type]], ...)
#             }else{
#               plotf(.parse(x, type), ...)
#             }
#           })


# setMethod("plot", "CalmrComparison", function(x, ...){
#   #try to make the matrix wide
#   dat = x@results %>% tidyr::pivot_wider(names_from = "model", values_from = "value")
#   if (any(is.na(dat))){
#     warning("Some data entries are not shared among all models and will be omitted.")
#     dat = stats::na.omit(dat)
#   }
#
#   GGally::ggpairs(dat, columns = x@model_layer_names,
#                   ggplot2::aes(colour = group),
#                   diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)))
#
# })

#' @export
setMethod("plot", "CalmrRSA",
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

setMethod("plot", "CalmrRSATest",
          function(x, ...){
            p = plot(x@RSA)
            sigmat = x@sig_mat
            sigmat[lower.tri(sigmat)] = NA
            dat = p$data
            dat$sig = data.frame(as.table(sigmat))$Freq
            p + ggplot2::geom_label(data = na.omit(dat[dat$sig,]), fill = "white")
          })

#' @export
graph <- function(x, ...) NULL
setMethod("graph", "CalmrModel", function(x, ...){
  if (x@is_parsed){
    graph_weights(x@model_results$vs, ...)
  }else{
    graph_weights(.parse(x, "vs"), ...)
  }
})

setMethod("graph", "CalmrExperiment", function(x, ...){
  dat = x@parsed_results$vs
  groups = unique(dat$group)
  ps = sapply(groups, function(g) graph_weights(dat[dat$group == g,], ...) +
                ggplot2::labs(title = sprintf("Group = %s", g)), simplify = F)
  names(ps) = groups
  ps
})

#### Predict methods ####
#' @export
setMethod("predict", "CalmrFit",
          function(object, type = "response", ...){
            prediction = object@model_function(object@model_pars, ...)
            if (type == "response"){
              prediction = object@link_function(prediction, object@link_pars)
            }
            prediction
          })

#### GOF methods ####
#' @export
NLL <- function(object, ...) NULL
setMethod("NLL", "CalmrFit", function(object){
  object@nloglik
})

#' @export
setMethod("AIC", "CalmrFit",
          function(object, ..., k = 2){
            2*k - 2*-object@nloglik
          })

#' @export
setMethod("BIC", "CalmrFit",
          function(object, ...){
            length(object@best_pars)*log(length(object@data)) -
              2*-object@nloglik
          })

