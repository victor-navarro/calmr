#' Plotting functions for heidi models
#' @return A list with ggplot objects
#' @seealso parse_heidi_results
#' @import magrittr
#' @name heidi_plots
NULL
#> NULL
#' @rdname heidi_plots
#' @param dat A list containing data.frames with parsed weights, r values, and v values, as returned by parse_heidi_results.
#' @param vals A data.frame containing parsed values
#' @param bars A logical stipulating whether to summarize and use stacked bars, instead of points and lines.
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
make_plots <- function(dat, filters){
  plotlist = list()
  for (g in unique(dat$ws$group)){
    plotlist[[paste0(g, ': Rs')]] = plot_rs(dat$rs %>% dplyr::filter(group == g)) + ggplot2::labs(title = g)
    plotlist[[paste0(g, ': Ws')]] = plot_ws(dat$ws %>% dplyr::filter(group == g)) + ggplot2::labs(title = g)
    plotlist[[paste0(g, ': Vs')]] = plot_vs(dat$vs %>% dplyr::filter(group == g)) + ggplot2::labs(title = g)
    plotlist[[paste0(g, ': Vs (bar)')]] = plot_vs(dat$vs %>% dplyr::filter(group == g), TRUE) + ggplot2::labs(title = g)
  }
  return(plotlist)
}
#' @rdname heidi_plots
#' @export
plot_ws <- function(vals){
  vals %>%
    dplyr::group_by(trial, phase, s1, s2) %>%
    dplyr::summarise(value = mean(value), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = trial, y = value, colour = s2)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_colour_discrete(drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::facet_wrap(~s1) +
    ggplot2::labs(x = 'Trial', y = 'Strength', colour = 'Predictee') +
    ggplot2::theme_bw()
}
#' @rdname heidi_plots
#' @export
plot_vs <- function(vals, bars = F){
  summ = vals %>%
    dplyr::group_by(trial, phase, trial_type, v_type, s1, s2) %>%
    dplyr::summarise(value = mean(value), .groups = "drop")
  if (!bars){
    plt = summ %>%
      ggplot2::ggplot(ggplot2::aes(x = trial, y = value, colour = s1, linetype = v_type, shape = v_type)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(fill = 'white') +
      ggplot2::scale_shape_manual(values = c(21, 16), drop = FALSE) +
      ggplot2::scale_colour_discrete(drop = FALSE) +
      ggplot2::scale_linetype_manual(values = c('dashed', 'solid'), drop = FALSE) +
      ggplot2::labs(x = 'Trial', y = 'V value', colour = 'Source',
                    shape = 'V type', linetype = 'V type') +
      ggplot2::scale_x_continuous(breaks = NULL) +
      ggplot2::theme_bw() +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
      ggplot2::facet_grid(s2~phase+trial_type, scales = 'free_x')
  }else{
    plt = summ %>%
      dplyr::group_by(phase, trial_type, v_type, s1, s2) %>%
      dplyr::summarise(value = mean(value), .groups = "drop") %>%
      ggplot2::ggplot(ggplot2::aes(x = trial_type, y = value, fill = s1, pattern = v_type)) +
      ggpattern::geom_bar_pattern(stat = "identity", colour = 'black',
                                  pattern_colour = NA, pattern_fill = 'white',
                                  pattern_spacing = 0.01) +
      ggpattern::scale_pattern_manual(values = c('stripe', 'none')) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
      ggplot2::facet_grid(s2~phase, scales = "free_x")
  }
  plt
}


#' @rdname heidi_plots
#' @export
plot_rs <- function(vals){
  vals %>%
    dplyr::group_by(trial, phase, trial_type, s1, s2) %>%
    dplyr::summarise(value = mean(value), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = trial, y = value, colour = s1)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
    ggplot2::geom_line() +
    ggbeeswarm::geom_beeswarm(groupOnX =FALSE) +
    ggplot2::scale_colour_discrete(drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::facet_grid(s2~phase+trial_type, scales = 'free_x') +
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
