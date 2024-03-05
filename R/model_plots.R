#' Create a plot with calmr data
#'
#' @param dat A tbl with data to use in the plot
#' @param type A character specifying the type of plot
#' @return A ggplot object
#' @note You should probably be getting plots via
#' the `plot` method for CalmrExperiments.
#' @export
#' @importFrom rlang .data

calmr_model_plot <- function(dat, type) {
  # define some big categories
  # exceptions are dealt with individually
  targetted <- c(
    "vs", "rs", "acts", "relacts", "psrcs",
    "m_ij", "ncs", "anccrs", "cws", "das", "qs", "ps"
  )
  singles <- c("as", "e_ij", "e_i", "m_i", "delta")
  # recalculate trial
  dat$trial <- ceiling(dat$trial / dat$block_size)
  # define geom layers
  line_point <- list(
    ggplot2::stat_summary(geom = "line", fun = "mean"),
    ggplot2::stat_summary(
      geom = "point",
      fun = "mean", position = ggbeeswarm::position_quasirandom()
    )
  )

  # Assemble aesthetics
  if (type %in% targetted) {
    .aes <- ggplot2::aes(
      x = .data$trial,
      y = .data$value,
      colour = .data$s2
    )
  }
  if (type %in% singles) {
    .aes <- ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$s1)
  }
  if (type %in% c("os")) {
    .aes <- ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$comp)
  }
  if ("type" %in% names(dat)) {
    .aes <- ggplot2::aes(
      x = .data$trial, y = .data$value,
      colour = .data$s2, linetype = type
    )
  }

  # Assemble geoms
  geoms <- line_point

  # Assemble labels
  labels <- list(ggplot2::labs(
    y = .get_prettyname(type),
    x = "Trial/Miniblock"
  ))
  if (type %in% targetted) {
    labels <- c(labels, list(ggplot2::labs(colour = "Target")))
  }
  if (type %in% singles) {
    labels <- c(labels, list(ggplot2::labs(colour = "Stimulus")))
  }
  if (type %in% c("os")) {
    labels <- c(labels, list(ggplot2::labs(colour = "Comparison")))
  }
  if ("type" %in% names(dat)) {
    labels <- c(labels, list(ggplot2::labs(linetype = "Type")))
  }

  # Assemble scales
  scales <- c(
    .calmr_scales("colour_d"),
    .calmr_scales("fill_d"),
    ggplot2::scale_x_continuous(breaks = NULL)
  )

  # Define grid
  grid <- list()
  if (type %in% targetted) {
    grid <- ggplot2::facet_grid(
      .data$s1 ~ .data$phase,
      scales = "free_x"
    )
  }
  if (type %in% c("acts", "relacts")) {
    grid <- ggplot2::facet_grid(
      .data$s2 ~ .data$phase + .data$trial_type,
      scales = "free_x"
    )
  }
  if (type %in% "os") {
    grid <- ggplot2::facet_grid(
      .data$s1 ~ .data$s2 + .data$phase,
      scales = "free_x", switch = "y"
    )
  }
  if (type %in% c("rs")) {
    grid <- ggplot2::facet_grid(
      .data$s1 ~ .data$phase +
        .data$trial_type,
      scales = "free_x"
    )
  }
  if (type %in% c("as")) {
    grid <- ggplot2::facet_grid(
      . ~ .data$phase +
        .data$trial_type,
      scales = "free_x"
    )
  }
  if (type %in% "psrcs") {
    grid <- ggplot2::facet_grid(
      .data$s1 + .data$type ~ .data$phase,
      scales = "free_x"
    )
  }

  ggplot2::ggplot(data = dat, mapping = .aes) +
    ggplot2::theme_bw() +
    geoms +
    labels +
    scales +
    grid
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
      ggplot2::scale_colour_viridis_c(begin = .1, end = .9, ...)
    },
    "fill_c" = {
      ggplot2::scale_fill_viridis_c(begin = .1, end = .9, ...)
    }
  )
}


.get_prettyname <- function(output) {
  prettynames <- c(
    "vs" = "Association Strength",
    "rs" = "Response Strength",
    "as" = "Saliency",
    "os" = "Switch Value",
    "eivs" = "Association Strength",
    "acts" = "Activation Strength",
    "relacts" = "Relative Activation",
    "e_ij" = "Event-contingent Eleg. Trace",
    "e_i" = "Eleg. Trace",
    "m_i" = "Baseline Predecessor Representation",
    "m_ij" = "Predecessor Representation",
    "ncs" = "Net Contingency",
    "anccrs" = "Adjusted Net Contingency",
    "delta" = "Time Delta",
    "psrcs" = "Representation Strength",
    "das" = "DA",
    "cws" = "Causal Weights",
    "qs" = "Action Value",
    "ps" = "Action Probabilities"
  )
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
#' @description Convenience function to patch plots with cowplot
#' @param plots A list of named plots, as returned by `calmr::plot`
#' @param selection A character or numeric vector determining the plots to patch
#' @param plot_options A list of plot options as returned by `get_plot_opts`
#' @export

patch_plots <- function(
    plots, selection = NULL,
    plot_options = get_plot_opts()) {
  # unlist plots
  pnames <- names(plots)

  if (is.null(selection)) {
    selection <- pnames
  }
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

  cow <- NULL
  if (length(selection)) {
    plots <- plots[selection]
    # if we want common scales
    if (plot_options$common_scale && length(selection) > 1) {
      plots <- plot_common_scale(plots)
    }
    cow <- cowplot::plot_grid(plotlist = plots)
  }
  cow
}
