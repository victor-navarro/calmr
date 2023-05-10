require(methods)

#### Generics ####
setGeneric("graph", function(x,...) standardGeneric("graph"))
setGeneric("NLL", function(object, ...) standardGeneric("NLL"))
setGeneric("get_output", function(object, ...) standardGeneric("get_output"))

#### Exposing methods ####
setMethod("show", "CalmrModel", function(object){
  cat(object@model, "run with:\n\n")
  print(object@parameters)
})
setMethod("show", "CalmrExperiment", function(object){
  summary(object)
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
  methods::show(object@RSA)
  cat("\nSignificance matrix:\n\n")
  print(object@sig_mat)
  cat(sprintf("\n%d permutation samples, two-tailed test with alpha = %1.2f.\n",
              object@n_samples, 1-object@p))
})

#### Accesing methods ####
#' Get output from CalmrExperiment
#'
#' Returns a tibble containing parsed outputs of CalmrExperiment
#'
#' @param object An object of class \code{\link{CalmrExperiment-class}}.
#' @param type The type of output
#' @return A data.frame with the model output
#' @export

get_output <- function(object, type = NULL) NULL
setMethod("get_output", "CalmrExperiment", function(object, type = NULL){
  if (is.null(type)){
    outputs = names(object@results$mod_data[[1]]@model_results) #works for parsed and non-parsed objects
    cat("Available model outputs:\n")
    cat(outputs, "\n\n")
    cat(sprintf("Use `get_output(model, type)` to access the outputs (e.g., `get_output(%s, '%s')`)", as.character(substitute(object)), outputs[1]), "\n")
  }else{
    if (!object@is_parsed){
      object = parse_experiment_results(object)
    }
    object@parsed_results[[type]]
  }

})



#### Summary methods ####
#' Summarise CalmrExperiment
#'
#' Shows a summary for a CalmrExperiment
#'
#' @param object An object of class \code{\link{CalmrExperiment-class}}.
#' @param ... Additional parameters passed to the summary function.
#' @return A
#' @export
setMethod("summary", "CalmrExperiment", function(object, ...){
  print(object@results$mod_data[[1]])
  outputs = names(object@results$mod_data[[1]]@model_results) #works for parsed and non-parsed objects
  cat("Available model outputs:\n")
  cat(outputs, "\n\n")
  cat(sprintf("Use `get_output(model, type)` to access the outputs (e.g., `get_output(%s, '%s')`)", as.character(substitute(object)), outputs[1]), "\n")
  #cat(sprintf("Use %s(model) "))
})

#### Ploting methods ####
#' Plot CalmrExperiment
#'
#' Creates a plot depicting results from CalmrExperiment
#'
#' @param x An object of class \code{\link{CalmrExperiment-class}}.
#' @param type A string specifying the type of plot to create. See ??supported_plots.
#' @param ... Additional parameters passed to the plotting function.
#' @return A ggplot object
#' @export
#' @rdname plot

setMethod("plot", "CalmrExperiment",
          function(x, type = NULL, y = NULL, ...){
            if (is.null(type)){ type = "vs"}
            if (!x@is_parsed){ x = parse_experiment_results(x)} #parse if model has not been parsed
            .calmr_check("supported_plot", type, names(x@parsed_results))
            plotinfo = .get_plot_functions(type)

            plotf = plotinfo[[type]]$fun
            if (type %in% c("evs", "ivs")){
              dat = rbind(data.frame(x@parsed_results[["evs"]], assoc_type = "Excitatory"),
                          data.frame(x@parsed_results[["ivs"]], assoc_type = "Inhibitory"))
            }else{
              dat = x@parsed_results[[type]]
            }

            groups = unique(dat$group)
            ps = sapply(groups, function(g) plotf(dat[dat$group == g,], ...) +
                          ggplot2::labs(title = sprintf("Group = %s", g)), simplify = F)
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

#' Plot RSA
#'
#' Plot a correlogram from RSA results
#'
#' @param x An object of class \code{\link{CalmrRSA-class}}.
#' @param ... Additional parameters passed to the plotting function.
#' @return A ggplot object
#' @export
setMethod("plot", "CalmrRSA",
          function(x, ...){
            corrmat = x@corr_mat
            corrmat[lower.tri(corrmat)] = NA
            dat = data.frame(as.table(corrmat))
            dat$label = round(dat$Freq, 2)
            dat %>% ggplot2::ggplot(ggplot2::aes(x = .data$Var1, y = .data$Var2,
                                                 fill = .data$Freq, label = .data$label)) +
              ggplot2::geom_tile(na.rm = T) +
              ggplot2::geom_text(na.rm = T) +
              ggplot2::scale_fill_gradient2(limits = c(-1, 1), na.value = "white") +
              ggplot2::theme(axis.title = ggplot2::element_blank(),
                             panel.background = ggplot2::element_blank()) +
              ggplot2::labs(fill = "Correlation") +
              ggplot2::scale_x_discrete(position = "top")
          })

#' Plot RSA test
#'
#' Plot a correlogram from RSA test results
#'
#' @param x An object of class \code{\link{CalmrRSATest-class}}.
#' @param ... Additional parameters passed to the plotting function.
#' @return A ggplot object
#' @export
setMethod("plot", "CalmrRSATest",
          function(x, ...){
            p = plot(x@RSA)
            sigmat = x@sig_mat
            sigmat[lower.tri(sigmat)] = NA
            dat = p$data
            dat$sig = data.frame(as.table(sigmat))$Freq
            p + ggplot2::geom_label(data = stats::na.omit(dat[dat$sig,]), fill = "white")
          })

#' Graph model associations
#'
#' Creates a network graph of model associations
#'
#' @param x An object of class \code{\link{CalmrModel-class}} or \code{\link{CalmrExperiment-class}}.
#' @param ... Additional parameters passed to the \code{\link{graph_weights}} function.
#' @return A ggplot object
#' @export
graph <- function(x, ...) NULL
setMethod("graph", "CalmrModel", function(x, ...){
  if (x@is_parsed){
    if (any(c("evs", "ivs") %in% names(x@model_results))){
      dat = x@model_results$evs
      dat$value = dat$value - x@model_results$ivs$value
      graph_weights(dat, ...)
    }else{
      graph_weights(x@model_results$vs, ...)
    }
  }else{
    # TODO: Implement this for unparsed models
    stop("The graph method requires a parsed model.")
  }
})
setMethod("graph", "CalmrExperiment", function(x, ...){
  if (any(c("evs", "ivs") %in% names(x@parsed_results))){
    dat = x@parsed_results$evs
    dat$value = dat$value - x@parsed_results$ivs$value
  }else{
    dat = x@parsed_results$vs
  }
  groups = unique(dat$group)
  ps = sapply(groups, function(g) graph_weights(dat[dat$group == g,], ...) +
                ggplot2::labs(title = sprintf("Group = %s", g)), simplify = F)
  names(ps) = groups
  ps
})

#### Predict methods ####
#' Predict from CalmrFit
#'
#' Obtain a prediction from CalmrFit
#'
#' @param object An object of class \code{\link{CalmrFit-class}}.
#' @param type A string. If `response`, model responses are transformed via the link function.
#' @param ... Additional parameters passed to the function (`object@model_function`).
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
#' NLL of CalmrFit
#'
#' Returns the negative log likelihood of a model fit via calmr::fit_model
#'
#' @param object An object of class \code{\link{CalmrFit-class}}.
#' @param ... Unused
#' @return A numeric
#' @export
NLL <- function(object, ...) NULL
setMethod("NLL", "CalmrFit", function(object){
  object@nloglik
})

#' AIC of CalmrFit
#'
#' Returns the Akaike Information Criterion of a model fit via calmr::fit_model
#'
#' @param object An object of class \code{\link{CalmrFit-class}}.
#' @param ... Unused
#' @param k Numeric. Penalty term (default = 2)
#' @return A numeric
#' @details The AIC is defined as `2*k - 2*-NLL`, where k is a penalty term and NLL is the negative log likelihood of the model.
#' @export
setMethod("AIC", "CalmrFit",
          function(object, ..., k = 2){
            k*length(object@best_pars) - 2*-object@nloglik
          })
#' BIC of CalmrFit
#'
#' Returns the Bayesian Information Criterion of a model fit via calmr::fit_model
#'
#' @param object An object of class \code{\link{CalmrFit-class}}.
#' @param ... Unused
#' @return A numeric
#' @details The AIC is defined as `k*log(n) - 2*-NLL`, where k is the number of parameters in the model and n is the number of observations
#' @export
setMethod("BIC", "CalmrFit",
          function(object, ...){
            length(object@best_pars)*log(length(object@data)) -
              2*-object@nloglik
          })


#### Comparison methods ####


