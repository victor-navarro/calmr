# A general plot for time data
plot_tbin <- function(dat, trial = max(dat$trial)) {
  ggplot2::ggplot(
    data = dat[dat$trial == trial, ],
    mapping = ggplot2::aes(
      x = .data$t_bin, y = .data$value,
      colour = .data$s2
    )
  ) +
    ggplot2::stat_summary(geom = "line", fun = "mean") +
    ggplot2::stat_summary(geom = "point", fun = "mean") +
    ggplot2::labs(x = sprintf("Time Bin (Trial: %d)", trial)) +
    ggplot2::theme_bw() +
    .calmr_scales("colour_d") +
    ggplot2::facet_grid(~ .data$s1)
}
