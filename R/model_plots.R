#' Create a plot with calmr data
#'
#' @param data A `data.table` containing aggregated
#' data from a [CalmrExperiment-class]
#' @param type A character specifying the type of plot.
#' @param model A character specifying the model.
#' @param ... Other parameters passed to plotting functions.
#' @return A 'ggplot' object.
#' @note You should probably be getting plots via
#' the [plot()] method for [CalmrExperiment-class].
#' @seealso [plotting_functions]
#' @export
#' @importFrom rlang .data

calmr_model_plot <- function(data, type, model, ...) {
  print(type)
  print(model)

  # Just serves the data depending on type/model
  plot_map <- list(
    "HDI2020" = list(
      "associations" = plot_targetted_trials,
      "pools" = plot_targetted_typed_trials,
      "responses" = plot_targetted_trials,
      "activations" = plot_trials
    ),
    "HD2022" = list(
      "activations" = .parse_2d,
      "pools" = .parse_ragged,
      "responses" = .parse_nd,
      "associations" = .parse_nd
    ),
    "RW1972" = list(
      "responses" = .parse_nd,
      "associations" = .parse_nd
    ),
    "MAC1975" = list(
      "responses" = .parse_nd,
      "associations" = .parse_nd,
      "associabilities" = .parse_2d
    ),
    "SM2007" = list(
      "activations" = .parse_nd,
      "relative_activations" = .parse_nd,
      "associations" = .parse_nd,
      "operator_switches" = .parse_nd
    ),
    "PKH1982" = list(
      "responses" = .parse_nd,
      "associabilities" = .parse_2d,
      "associations" = .parse_typed
    ),
    "ANCCR" = list(
      "ij_elegibilities" = .parse_2d,
      "i_elegibilities" = .parse_2d,
      "i_base_rate" = .parse_2d,
      "ij_base_rate" = .parse_nd,
      "representation_contingencies" = .parse_typed,
      "net_contingencies" = .parse_nd,
      "anccrs" = .parse_nd,
      "causal_weights" = .parse_nd,
      "dopamines" = .parse_nd,
      "action_values" = .parse_nd,
      "probabilities" = .parse_nd
    ),
    "TD" = list(
      "associations" = plot_targetted_tbins,
      "values" = plot_tbins,
      "elegibilities" = plot_tbins
    ),
    "RAND" = list(
      "responses" = .parse_nd,
      "associations" = .parse_nd
    )
  )
  p <- do.call(plot_map[[model]][[type]], c(list(data = data), ...))
  p <- p + ggplot2::labs(
    y = .get_y_prettyname(type),
    colour = .get_scale_prettyname(type),
    fill = .get_scale_prettyname(type)
  )

  p
  # # define some big categories
  # # exceptions are dealt with individually
  # targetted <- c(
  #   "vs", "rs", "acts", "heidi_acts", "relacts", "psrcs",
  #   "m_ij", "ncs", "anccrs", "cws", "das", "qs", "ps"
  # )
  # singles <- c("as", "e_ij", "e_i", "m_i", "delta")
}

# internal function to define and make scales available
.calmr_scales <- function(which, ...) {
  switch(which,
    "colour_d" = {
      ggplot2::scale_colour_viridis_d(begin = .1, end = .9, ...)
    },
    "fill_d" = {
      ggplot2::scale_fill_viridis_d(begin = .1, end = .9, ...)
    },
    "colour_c" = {
      ggplot2::scale_colour_viridis_c(begin = .1, end = .9, ...) # nocov
    },
    "fill_c" = {
      ggplot2::scale_fill_viridis_c(begin = .1, end = .9, ...)
    }
  )
}


.get_y_prettyname <- function(output) {
  prettynames <- c(
    "associations" = "Association Strength",
    "responses" = "Response Strength",
    "pools" = "Pooled Association Strength",
    "associabilities" = "Associability",
    "operator_switches" = "Switch Value",
    "activations" = "Activation Strength",
    "relative_activations" = "Relative Activation Strength",
    "ij_elegibilities" = "Event-contingent Eleg. Trace",
    "i_elegibilities" = "Eleg. Trace",
    "i_base_rate" = "Baseline Predecessor Representation",
    "ij_base_rate" = "Predecessor Representation",
    "net_contingencies" = "Net Contingency Strength",
    "anccrs" = "Adjusted Net Contingency Strength",
    "representation_contingencies" = "Representation Strength",
    "dopamines" = "DA Strength",
    "causal_weights" = "Causal Weights",
    "action_values" = "Action Value",
    "action_probabilities" = "Action Probabilities",
    "values" = "Expected Value",
    "elegibilities" = "Elegibility Trace"
  )
  if (!(output %in% names(prettynames))) browser() # return(list())
  ggplot2::labs(y = prettynames[output])
}

.get_scale_prettyname <- function(output) {
  prettynames <- c(
    "operator_switches" = "Comparison",
    "associations" = "Target",
    "values" = "Target",
    "elegibilities" = "Stimulus"
  )
  # if (type %in% targetted) {
  #   labels <- c(labels, list(ggplot2::labs(colour = "Target")))
  # }
  # if (type %in% singles) {
  #   labels <- c(labels, list(ggplot2::labs(colour = "Stimulus")))
  # }
  # if ("type" %in% names(data)) {
  #   labels <- c(labels, list(ggplot2::labs(linetype = "Type")))
  # }
  prettynames[output]
}

plot_common_scale <- function(plots) {
  # get min and max y-scale
  ranges <- unlist(lapply(plots, function(p) {
    ggplot2::layer_scales(p)$y$range$range
  }))
  miny <- min(ranges)
  maxy <- max(ranges)
  for (p in seq_len(length(plots))) {
    plots[[p]] <- plots[[p]] +
      ggplot2::coord_cartesian(ylim = c(miny, maxy))
  }
  plots
}

get_plot_opts <- function(common_scale = TRUE) {
  return(list(common_scale = common_scale))
}

#' Patch Calmr plots
#'
#' @description Convenience function to patch plots with `patchwork`
#' @param plots A list of named plots, as returned by `calmr::plot()`
#' @param selection A character or numeric vector determining the plots to patch
#' @param plot_options A list of plot options as returned by [get_plot_opts()]
#' @export
#' @return A `patchwork` object

patch_plots <- function(
    plots, selection = names(plots),
    plot_options = get_plot_opts()) {
  # unlist plots
  pnames <- names(plots)
  if (all(is.character(selection))) {
    if (!all(selection %in% pnames)) {
      stop("Selection must match names in plots")
    }
  }
  if (all(is.numeric(selection))) {
    if (!all(selection %in% seq_len(length(pnames)))) {
      stop("Selection indices exceed the number of plots")
    }
    selection <- pnames[selection]
  }

  patch <- NULL
  if (length(selection)) {
    plots <- plots[selection]
    # if we want common scales
    if (plot_options$common_scale && length(selection) > 1) {
      plots <- plot_common_scale(plots)
    }
    patch <- patchwork::wrap_plots(plots)
  }
  patch
}
