#' Plotting functions for heidi models
#' @return A list with ggplot objects
#' @seealso parse_heidi_results
#' @import magrittr
#' @name heidi_plots
NULL
#> NULL
#' @rdname heidi_plots
#' @param dat A list containing data.frames with parsed weights, r values, and v values, as returned by parse_heidi_results.
#' @param plots A named list with plots
#' @param selection A character vector with the selected plots
#' @param options A list with options
#' @param common_scalle A logical. Whether to plot the data in a common y-scale.
#' @note
#' \itemize{
#' \item{Plotting options are obtained via get_plot_opts(). For now, only plotting in a common y-axis is supported.
#' }}
#' @import patchwork
#' @export
make_plots <- function(dat){
  plotlist = list()
  for (g in unique(dat$ws$group)){
    plotlist[[paste(g, 'Distributed Rs')]] = plot_rs(dat$rs[dat$rs$group == g, ]) + ggplot2::labs(title = g)
    plotlist[[paste(g, 'S-S')]] = plot_weights(dat$ws[dat$ws$group == g, ]) + ggplot2::labs(title = g)
    plotlist[[paste(g, 'Split Vs')]] = plot_vs(dat$vs[dat$vs$group == g, ]) + ggplot2::labs(title = g)
  }
  return(plotlist)
}
#' @rdname heidi_plots
#' @param vals A data.frame containing parsed values
#' @export
plot_weights <- function(vals){
  vals %>%
    dplyr::group_by(trial, s1, s2) %>%
    dplyr::summarise(value = mean(value), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = trial, y = value, colour = s1)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
    ggplot2::scale_colour_discrete(drop = FALSE) +
    ggplot2::facet_wrap(~s2) +
    ggplot2::labs(x = 'Trial', y = 'Strength', colour = 'Predictor') +
    ggplot2::theme_bw()
}
#' @rdname heidi_plots
#' @export
plot_vs <- function(vals){
  vals %>%
    dplyr::group_by(trial, trial_type, v_type, s1, s2) %>%
    dplyr::summarise(value = mean(value), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = trial, y = value, colour = trial_type, linetype = v_type, shape = v_type)) +
    ggplot2::geom_line() + ggplot2::geom_point(fill = 'white') +
    ggplot2::scale_shape_manual(values = c(16, 21), drop = FALSE) +
    ggplot2::scale_colour_discrete(drop = FALSE) +
    ggplot2::scale_linetype_discrete(drop = FALSE) +
    ggplot2::labs(x = 'Trial', y = 'V value', colour = 'Trial Type',shape = 'V type', linetype = 'V type') +
    ggplot2::facet_grid(s1~s2) +
    ggplot2::theme_bw()
}

#' @rdname heidi_plots
#' @export
plot_rs <- function(vals){
  vals %>%
    dplyr::group_by(trial, s1, s2, trial_type) %>%
    dplyr::summarise(value = mean(value), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = trial, y = value, colour = s1)) +
    ggplot2::geom_line() +  ggbeeswarm::geom_beeswarm(groupOnX =FALSE) +
    ggplot2::scale_colour_discrete(drop = FALSE) +
    ggplot2::facet_grid(s2~trial_type, scales = 'free_x') +
    ggplot2::labs(x = 'Trial', y = 'R value', colour = 'Stimulus') +
    ggplot2::theme_bw()
}
#' @rdname heidi_plots
#' @export
patch_plots <- function(plots, selection, options = get_plot_opts()){
  patch = ggplot2::ggplot()
  selected = length(selection)
  if (selected){
    #if we want common scales
    if (options$common_scale & selected > 1){
      #get min and max y-scale
      ranges = unlist(lapply(plots[selection], function(p) ggplot2::layer_scales(p)$y$range$range))
      miny = min(ranges)
      maxy = max(ranges)
      for (p in selection[1:selected]){
        plots[[p]] = plots[[p]] + ggplot2::coord_cartesian(ylim = c(miny, maxy))
      }
    }

    patch = plots[[selection[1]]]
    if (selected > 1){
      for (p in selection[2:selected]){
        patch = patch + plots[p]
      }
    }
  }
  return(patch + patchwork::plot_layout(guides = 'collect'))
}

#' @rdname heidi_plots
#' @export
get_plot_opts <- function(common_scale = TRUE){
  return(list(common_scale = common_scale))
}
