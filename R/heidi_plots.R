#' Plotting functions for heidi models
#' @return A list with ggplot objects
#' @seealso parse_heidi_results
#' @import magrittr
#' @name heidi_plots
NULL
#> NULL
#' @rdname heidi_plots
#' @param dat A list containing data.frames with parsed weights, r values, and activation values, as returned by parse_heidi_results.
#' @param vals A data.frame containing parsed values
#' @param simple A logical stipulating whether to simplify the plot by collapsing across sources.
#' @param bars A logical stipulating whether to summarize and use stacked bars, instead of points and lines.
#' @param plots A named list with plots
#' @param selection A character vector with the selected plots
#' @param options A list with options
#' @param common_scale A logical. Whether to plot the data in a common y-scale.
#' @param vs A data.frame containing parsed weights, as returned by parse_heidi_results
#' @param trial An integer denoting the trial of the weights to be graphed. Defaults to the last trial in the data.
#' @param limits A vector of length 2 specifying the range of weights. Defaults to the negative and positive maximum of absolute weights.
#' @param t An integer specifying a trial
#' @param opts A list of options for graphing weights. See ?get_graph_opts.
#' @param graph_size A string specifying the desired graph size, from c("large", "small"). Default is "large"
#' @param mod Parsed model results, as returned by parse_heidi_results.
#' @param graphs A list of graphs, as returned by make_graphs
#' @note
#' \itemize{
#' \item{Plotting options are obtained via get_plot_opts(). For now, only plotting in a common y-axis is supported.
#' }}
#' @export
make_plots <- function(dat){
  plotlist = list()
  for (g in unique(dat$vs$group)){
    plotlist[[paste0(g, ': Rs (simple)')]] = plot_rs(dat$rs %>% dplyr::filter(.data$group == g), simple = TRUE) +
      ggplot2::labs(title = paste0(g, ': Rs (simple)'))
    plotlist[[paste0(g, ': Rs (complex)')]] = plot_rs(dat$rs %>% dplyr::filter(.data$group == g), simple = FALSE) +
      ggplot2::labs(title = paste0(g, ': Rs (complex)'))
    plotlist[[paste0(g, ': Acts (bar)')]] = plot_acts(dat$acts %>% dplyr::filter(.data$group == g), bars = TRUE) +
      ggplot2::labs(title = paste0(g, ': Acts (bar)'))
    plotlist[[paste0(g, ': Acts (learning)')]] = plot_acts(dat$acts %>% dplyr::filter(.data$group == g)) +
      ggplot2::labs(title = paste0(g, ': Acts (learning)'))
    plotlist[[paste0(g, ': Vs')]] = plot_vs(dat$vs %>% dplyr::filter(.data$group == g)) +
      ggplot2::labs(title = paste0(g, ': Vs'))
    plotlist[[paste0(g, ': As')]] = plot_as(dat$as %>% dplyr::filter(.data$group == g)) +
      ggplot2::labs(title = paste0(g, ': As'))
  }
  return(plotlist)
}
#' @rdname heidi_plots
#' @export
plot_vs <- function(vals){
  vals %>%
    dplyr::mutate(trial = ceiling(.data$trial/.data$block_size)) %>%
    dplyr::group_by(.data$trial, .data$phase, .data$s1, .data$s2) %>%
    dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$s2)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
    ggplot2::geom_line() +
    ggbeeswarm::geom_beeswarm(groupOnX =FALSE) +
    ggplot2::scale_colour_viridis_d(drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::facet_grid(.data$s1~.data$phase, scales = 'free_x') +
    ggplot2::labs(x = "Trial/Miniblock", y = 'Strength', colour = 'Predictee') +
    ggplot2::theme_bw()
}
#' @rdname heidi_plots
#' @export
plot_acts <- function(vals, bars = F){
  summ = vals %>%
    dplyr::mutate(trial = ceiling(.data$trial/.data$block_size)) %>%
    dplyr::group_by(.data$trial, .data$phase,
                    .data$trial_type, .data$act_type, .data$s1, .data$s2) %>%
    dplyr::summarise(value = mean(.data$value), .groups = "drop")
  if (!bars){
    plt = summ %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$trial, y = .data$value,
                                   colour = .data$s1, linetype = .data$act_type,
                                   shape = .data$act_type)) +
      ggplot2::geom_line() +
      ggbeeswarm::geom_beeswarm(groupOnX =FALSE, fill = "white") +
      ggplot2::scale_shape_manual(values = c(21, 16), drop = FALSE) +
      ggplot2::scale_colour_viridis_d(drop = FALSE) +
      ggplot2::scale_linetype_manual(values = c('dashed', 'solid'), drop = FALSE) +
      ggplot2::labs(x = "Trial/Miniblock", y = 'V value', colour = 'Source',
                    shape = 'V type', linetype = 'V type') +
      ggplot2::scale_x_continuous(breaks = NULL) +
      ggplot2::theme_bw() +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
      ggplot2::facet_grid(.data$s2~.data$phase+.data$trial_type, scales = 'free_x')
  }else{
    plt = summ %>%
      dplyr::group_by(.data$phase, .data$trial_type, .data$act_type,
                      .data$s1, .data$s2) %>%
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$trial_type, y = .data$value,
                                   fill = .data$s1, alpha = .data$act_type)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::stat_summary(ggplot2::aes(group = 1), geom = "point", fun = "sum") +
      ggplot2::scale_alpha_manual(values = c(.5, 1)) +
      ggplot2::labs(x = "Trial/Miniblock", y = 'V value', fill = 'Source',
                    pattern = 'V type', alpha = 'V type') +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_viridis_d(drop = FALSE) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
      ggplot2::facet_grid(.data$s2~.data$phase, scales = "free_x", space = 'free_x')
  }
  plt
}


#' @rdname heidi_plots
#' @export
plot_rs <- function(vals, simple = F){
  summ = vals %>%
    dplyr::mutate(trial = ceiling(.data$trial/.data$block_size)) %>%
    dplyr::group_by(.data$trial, .data$phase, .data$trial_type, .data$s1, .data$s2) %>%
    dplyr::summarise(value = mean(.data$value), .groups = "drop")
  if (simple){
    plt = summ %>%
      dplyr::group_by(.data$trial, .data$phase, .data$trial_type, .data$s1) %>%
      dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$s1)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
      ggplot2::geom_line() +
      ggbeeswarm::geom_beeswarm(groupOnX =FALSE) +
      ggplot2::scale_colour_viridis_d(drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = NULL) +
      ggplot2::facet_grid(~.data$phase+.data$trial_type, scales = 'free_x') +
      ggplot2::labs(x = "Trial/Miniblock", y = 'R value', colour = 'Stimulus') +
      ggplot2::theme_bw()

  }else{
    plt = summ %>% ggplot2::ggplot(ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$s1)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
      ggplot2::geom_line() +
      ggbeeswarm::geom_beeswarm(groupOnX =FALSE) +
      ggplot2::scale_colour_viridis_d(drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = NULL) +
      ggplot2::facet_grid(.data$s2~.data$phase+.data$trial_type, scales = 'free_x') +
      ggplot2::labs(x = "Trial/Miniblock", y = 'R value', colour = 'Target') +
      ggplot2::theme_bw()
  }
  plt
}

#' @rdname heidi_plots
#' @export
plot_as <- function(vals){
  vals %>%
    dplyr::mutate(trial = ceiling(.data$trial/.data$block_size)) %>%
    dplyr::group_by(.data$trial, .data$phase, .data$trial_type, .data$s1) %>%
    dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$s1)) +
    ggplot2::geom_line() +
    ggbeeswarm::geom_beeswarm(groupOnX =FALSE) +
    ggplot2::scale_colour_viridis_d(drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::facet_grid(.~.data$phase+.data$trial_type, scales = 'free_x') +
    ggplot2::labs(x = "Trial/Miniblock", y = 'Alpha Value', colour = 'Stimulus') +
    ggplot2::theme_bw()
}

#' @rdname heidi_plots
#' @export
get_plot_opts <- function(common_scale = TRUE){
  return(list(common_scale = common_scale))
}

#' @rdname heidi_plots
#' @export
plot_common_scale <- function(plots){
  #get min and max y-scale
  ranges = unlist(lapply(plots, function(p) ggplot2::layer_scales(p)$y$range$range))
  miny = min(ranges)
  maxy = max(ranges)
  for (p in 1:length(plots)){
    plots[[p]] = plots[[p]] + ggplot2::coord_cartesian(ylim = c(miny, maxy))
  }
  plots
}

#' @rdname heidi_plots
#' @export
graph_weights <- function(vs, limits = max(abs(range(vs$value)))*c(-1, 1),
                          t = max(vs$trial), opts = get_graph_opts()){
  vs = vs %>% dplyr::filter(.data$trial == t) %>%
    dplyr::group_by(.data$s1, .data$s2) %>%
    dplyr::summarise(value = mean(.data$value)) %>%
    dplyr::mutate(s1 = as.character(.data$s1), s2 = as.character(.data$s2)) %>%
    dplyr::rename(from = .data$s1, to = .data$s2, weight = .data$value) %>%
    as.data.frame()
  net = ggnetwork::ggnetwork(network::as.network(vs),
                             layout = "circle",
                             arrow.gap = opts$arrow.gap)
  ggplot2::ggplot(net, ggplot2::aes(x = .data$x, y = .data$y,
                                    xend = .data$xend, yend = .data$yend,
                                    colour = .data$weight,
                                    label = .data$vertex.names)) +
    ggnetwork::geom_edges(curvature = opts$arrow.curvature,
                          size = opts$edge.size,
                          arrow = grid::arrow(length = grid::unit(opts$arrow.pt, "pt"), type = "closed")) +
    ggnetwork::geom_nodes(size = opts$node.size, pch = 21, colour = 'black',
                          fill = 'white', stroke = opts$node.stroke) +
    ggnetwork::geom_nodetext(size = opts$node.text.size, colour = "black") +
    ggplot2::scale_colour_gradient2(high = "#fde725", low = "#440154", mid = "white", midpoint = 0, limits = limits) +
    ggplot2::theme_void() +
    ggplot2::guides(colour = "none") +
    ggplot2::coord_cartesian(xlim = c(-0.2, 1.2), ylim = c(-0.2, 1.2))
}

#' @rdname heidi_plots
#' @export
get_graph_opts <- function(graph_size = "small"){
  if (graph_size == "large"){
    arrow.gap = 0.16
    arrow.curvature = 0.2
    arrow.pt = 20
    edge.size = 3
    node.size = 40
    node.stroke = 3
    node.text.size = 15
  }
  if (graph_size == "small"){
    arrow.gap = 0.10
    arrow.curvature = 0.2
    arrow.pt = 10
    edge.size = 1.5
    node.size = 20
    node.stroke = 1.5
    node.text.size = 7.5

  }
  return(list(arrow.gap = arrow.gap,
              arrow.curvature = arrow.curvature,
              arrow.pt = arrow.pt,
              edge.size = edge.size,
              node.size = node.size,
              node.stroke = node.stroke,
              node.text.size = node.text.size))

}

#' @rdname heidi_plots
#' @export
make_graphs <- function(mod,
                        limits = max(abs(range(mod$vs$value)))*c(-1, 1),
                        trial = max(mod$vs$trial),
                        opts = get_graph_opts()){
  plotlist = list()
  for (g in unique(mod$vs$group)){
    plotlist[[sprintf('Group %s: (Trial %d)', g, trial)]] = graph_weights(mod$vs %>% dplyr::filter(.data$group == g),
                                                                          t = trial,
                                                                          limits = limits,
                                                                          opts = opts) +
      ggplot2::labs(title = sprintf('%s (Trial %d)', g, trial))
  }
  return(plotlist)
}

#' @rdname heidi_plots
#' @export
patch_graphs <- function(graphs, selection = names(graphs)){
  #expects a list of graphs via make_graphs/graph_weights
  graphs = graphs[selection]
  cow = cowplot::plot_grid(plotlist = graphs)
  cow
}

#' @rdname heidi_plots
#' @export
patch_plots <- function(plots, selection, options = get_plot_opts()){
  cow = NULL
  selected = length(selection)
  if (selected){
    plots = plots[selection]
    #if we want common scales
    if (options$common_scale & selected > 1){
      plots = plot_common_scale(plots)
    }
    cow = cowplot::plot_grid(plotlist = plots)
  }
  cow
}
