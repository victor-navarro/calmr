#' General plotting options
#' @name plotting_options
NULL
#> NULL

#' @description `plot_common_scale()` rescales a list of
#' plots to have a common scale.
#' @param plots A list of (named) plots, as returned by [plot()].
#' @returns `plot_common_scale()` returns a list of plots.
#' @aliases plot_common_scale
#' @rdname plotting_options
#' @export
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

#' @description `get_plot_opts()` returns generic plotting options.
#' @param common_scale Logical specifying whether to
#' have plots in a common scale.
#' @return `get_plot_opts()` returns a list.
#' @aliases get_plot_opts
#' @export
#' @rdname plotting_options
get_plot_opts <- function(common_scale = TRUE) {
  return(list(common_scale = common_scale))
}

#' @description `patch_plots()` patches plots using `patchwork` package.
#' @param selection A character or numeric vector determining the plots to patch
#' @param plot_options A list of plot options as returned by [get_plot_opts()]
#' @aliases patch_plots
#' @rdname plotting_options
#' @export
#' @return `patch_plots()` returns a `patchwork` object.

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
