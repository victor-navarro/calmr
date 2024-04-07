#' General plotting functions
#' @param data A `data.frame`-like with data to plot.
#' @param t A numeric vector specifying the trial(s) to plot.
#' Defaults to the last trial in data.
#' @name plotting_functions
#' @note All data must be organised as
#' returned by [results()] or [parsed_results()].
#' @importFrom rlang .data
NULL
#> NULL


#' @description `plot_targetted_tbins()` plots targetted time data on a trial.
#' @returns `plot_targetted_tbins()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export

# A general plot for targetted (s2) time data
plot_targetted_tbins <- function(data, t = max(data$trial)) {
  ggplot2::ggplot(
    data = data[data$trial == t, ],
    mapping = ggplot2::aes(
      x = .data$t_bin, y = .data$value,
      colour = .data$s2
    )
  ) +
    ggplot2::stat_summary(geom = "line", fun = "mean") +
    ggplot2::stat_summary(geom = "point", fun = "mean") +
    ggplot2::labs(x = sprintf("Time Bin (Trial: %d)", t)) +
    ggplot2::theme_bw() +
    .calmr_scales("colour_d") +
    ggplot2::facet_grid(.data$s1 ~ .data$phase + .data$trial_type)
}

#' @description `plot_tbins()` plots non-targetted time data on a trial.
#' @returns `plot_tbins()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export

# A general plot for non-targetted time data
plot_tbins <- function(data, t = max(data$trial)) {
  ggplot2::ggplot(
    data = data[data$trial %in% t, ],
    mapping = ggplot2::aes(
      x = .data$t_bin, y = .data$value,
      colour = .data$s1
    )
  ) +
    ggplot2::stat_summary(geom = "line", fun = "mean") +
    ggplot2::stat_summary(geom = "point", fun = "mean") +
    ggplot2::labs(x = sprintf("Time Bin (Trial: %d)", t)) +
    ggplot2::theme_bw() +
    .calmr_scales("colour_d") +
    ggplot2::facet_grid(~ .data$phase + .data$trial_type + .data$trial)
}

#' @description `plot_targetted_trials()` plots targetted trial data.
#' @returns `plot_targetted_trials()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export

# A general plot for trial-based, targetted data
plot_targetted_trials <- function(data) {
  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = ceiling(.data$trial / .data$block_size), y = .data$value,
      colour = .data$s2
    )
  ) +
    ggplot2::stat_summary(geom = "line", fun = "mean") +
    ggplot2::stat_summary(geom = "point", fun = "mean") +
    ggplot2::labs(x = "Trial/Miniblock") +
    ggplot2::theme_bw() +
    .calmr_scales("colour_d") +
    ggplot2::facet_grid(.data$s1 ~ .data$phase + .data$trial_type,
      scales = "free_x"
    )
}
#' @description `plot_trials()` plots non-targetted trial data.
#' @returns `plot_trials()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export
# A general plot for trial-based, targetted data
plot_trials <- function(data) {
  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = ceiling(.data$trial / .data$block_size), y = .data$value,
      colour = .data$s1
    )
  ) +
    ggplot2::stat_summary(geom = "line", fun = "mean") +
    ggplot2::stat_summary(geom = "point", fun = "mean") +
    ggplot2::labs(x = "Trial/Miniblock") +
    ggplot2::theme_bw() +
    .calmr_scales("colour_d") +
    ggplot2::facet_grid(~ .data$phase + .data$trial_type,
      scales = "free_x"
    )
}

#' @description `plot_targetted_typed_trials()` plots
#' targetted trial data with a type.
#' @returns `plot_targetted_typed_trials()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export
# A general plot for trial-based, targetted data
plot_targetted_typed_trials <- function(data) {
  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = ceiling(.data$trial / .data$block_size), y = .data$value,
      colour = .data$s2, linetype = .data$type
    )
  ) +
    ggplot2::stat_summary(geom = "line", fun = "mean") +
    ggplot2::stat_summary(geom = "point", fun = "mean") +
    ggplot2::labs(x = "Trial/Miniblock", linetype = "Type") +
    ggplot2::theme_bw() +
    .calmr_scales("colour_d") +
    ggplot2::facet_grid(.data$s1 ~ .data$phase + .data$trial_type,
      scales = "free_x"
    )
}

#' @description `plot_targetted_complex_trials()` plots
#' targetted data with a third variable.
#' @param col A string specifying the column of the third variable.
#' @returns `plot_targetted_complex_trials()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export
# A general plot for trial-based, targetted data with an extra column.
plot_targetted_complex_trials <- function(data, col) {
  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = ceiling(.data$trial / .data$block_size), y = .data$value,
      colour = .data$s2, linetype = .data[[col]]
    )
  ) +
    ggplot2::stat_summary(geom = "line", fun = "mean") +
    ggplot2::stat_summary(geom = "point", fun = "mean") +
    ggplot2::labs(x = "Trial/Miniblock", linetype = tools::toTitleCase(col)) +
    ggplot2::theme_bw() +
    .calmr_scales("colour_d") +
    ggplot2::facet_grid(.data$s1 ~ .data$phase + .data$trial_type,
      scales = "free_x"
    )
}

#' Get calmr scales
#' @param which A string specifying the scale
#' @param ... Other parameters passed to the corresponding scale function.
#' @return A 'ggplot2' scale for colour or fill.
#' @noRd
.calmr_scales <- function(which, ...) {
  current_scale <- getOption("calmr_palette", default = "viridis")
  if (current_scale == "viridis") {
    return(switch(which,
      "colour_d" = ggplot2::scale_colour_viridis_d(
        begin = .1, end = .9, ...
      ),
      "fill_d" = ggplot2::scale_fill_viridis_d(
        begin = .1, end = .9, ...
      ),
      "colour_c" = ggplot2::scale_colour_viridis_c(
        begin = .1, end = .9, ...
      ),
      "fill_c" = ggplot2::scale_fill_viridis_c(
        begin = .1, end = .9, ...
      )
    ))
  }
  if (current_scale == "hue") {
    return(switch(which,
      "colour_d" = ggplot2::scale_colour_discrete(...),
      "fill_d" = ggplot2::scale_fill_discrete(...),
      "colour_c" = ggplot2::scale_colour_continuous(...), # nocov
      "fill_c" = ggplot2::scale_fill_continuous(...)
    ))
  }
}
