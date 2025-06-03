#' General plotting functions
#' @param data A `data.frame`-like with data to plot.
#' @param t A numeric vector specifying the trial(s) to plot.
#' Defaults to the last trial in data.
#' @name plotting_functions
#' @note These functions are not meant to be used by non-developers.
#' If you want to plot data from a model or an experiment,
#' see the `plot()` method.
#' All data must be parsed or aggregated, as
#' returned by [results()] or [parsed_results()].
#' @importFrom rlang .data
NULL
#> NULL

#' @description `plot_targeted_tbins()` plots targeted time data on a trial.
#' @returns `plot_targeted_tbins()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export

# A general plot for targeted (s2) time data
plot_targeted_tbins <- function(data, t = max(data$trial)) {
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

#' @description `plot_tbins()` plots non-targeted time data on a trial.
#' @returns `plot_tbins()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export

# A general plot for non-targeted time data
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

#' @description `plot_targeted_trials()` plots targeted trial data.
#' @returns `plot_targeted_trials()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export

# A general plot for trial-based, targeted data
plot_targeted_trials <- function(data) {
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
#' @description `plot_trials()` plots non-targeted trial data.
#' @returns `plot_trials()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export
# A general plot for trial-based, targeted data
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

#' @description `plot_targeted_typed_trials()` plots
#' targeted trial data with a type.
#' @returns `plot_targeted_typed_trials()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export
# A general plot for trial-based, targeted data
plot_targeted_typed_trials <- function(data) {
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

#' @description `plot_targeted_complex_trials()` plots
#' targeted data with a third variable.
#' @param col A string specifying the column of the third variable.
#' @returns `plot_targeted_complex_trials()` returns 'ggplot' object.
#' @rdname plotting_functions
#' @export
# A general plot for trial-based, targeted data with an extra column.
plot_targeted_complex_trials <- function(data, col) {
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

.get_y_prettyname <- function(output) {
  prettynames <- c(
    "associations" = "Association Strength",
    "responses" = "Response Strength",
    "pools" = "Pooled Association Strength",
    "associabilities" = "Associability",
    "operator_switches" = "Switch Value",
    "activations" = "Activation Strength",
    "relative_activations" = "Relative Activation Strength",
    "ij_eligibilities" = "Event-contingent Eleg. Trace",
    "i_eligibilities" = "Eleg. Trace",
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
    "eligibilities" = "Eligibility Trace",
    "probabilities" = "Response Probability"
  )
  prettynames[output]
}

.get_scale_prettyname <- function(output) {
  prettynames <- c(
    "responses" = "Target",
    "operator_switches" = "Target",
    "associations" = "Target",
    "associabilities" = "Stimulus",
    "values" = "Target",
    "eligibilities" = "Stimulus",
    "activations" = "Target",
    "pools" = "Target",
    "relative_activations" = "Target",
    "ij_eligibilities" = "Stimulus",
    "i_eligibilities" = "Stimulus",
    "i_base_rate" = "Stimulus",
    "ij_base_rate" = "Target",
    "net_contingencies" = "Target",
    "anccrs" = "Target",
    "representation_contingencies" = "Target",
    "dopamines" = "Target",
    "causal_weights" = "Target",
    "action_values" = "Target",
    "action_probabilities" = "Target",
    "values" = "Target",
    "eligibilities" = "Stimulus",
    "probabilities" = "Target"
  )
  prettynames[output]
}
