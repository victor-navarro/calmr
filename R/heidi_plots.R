#' Plotting functions for heidi models
#' @return A list with ggplot objects
#' @seealso parseHeidiResults
#' @import magrittr
#' @name heidi_plots
NULL
#> NULL
#' @rdname heidi_plots
#' @param dat A list containing data.frames with parsed weights, r values, and v values, as returned by parseHeidiResults.
#' @export
makePlots <- function(dat){
  plotlist = list()
  for (g in unique(dat$ws$group)){
    plotlist[[paste(g, 'Distributed Rs')]] = plotRs(dat$rs[dat$rs$group == g, ]) + ggplot2::labs(title = g)
    plotlist[[paste(g, 'S-S')]] = plotWeights(dat$ws[dat$ws$group == g, ]) + ggplot2::labs(title = g)
    plotlist[[paste(g, 'Split Vs')]] = plotVs(dat$vs[dat$vs$group == g, ]) + ggplot2::labs(title = g)
  }
  return(plotlist)
}
#' @rdname heidi_plots
#' @param vals A data.frame containing parsed values
#' @export
plotWeights <- function(vals){
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
plotVs <- function(vals){
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
plotRs <- function(vals){
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
