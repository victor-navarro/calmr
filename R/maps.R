formula_map <- function() {
  list(
    "ANCCR" = list(
      "ij_eligibilities" = c(),
      "i_eligibilities" = c(),
      "i_base_rate" = c(),
      "ij_base_rate" = "s2",
      "representation_contingencies" = c("s2", "type"),
      "net_contingencies" = "s2",
      "anccrs" = "s2",
      "causal_weights" = "s2",
      "dopamines" = "s2",
      "action_values" = "s2",
      "probabilities" = "s2"
    ),
    "TD" = list(
      "associations" = c("s2", "t_bin"),
      "eligibilities" = "t_bin",
      "values" = "t_bin"
    )
  )
}

parse_map <- function() {
  list(
    "ANCCR" = list(
      "ij_eligibilities" = .parse_2d,
      "i_eligibilities" = .parse_2d,
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
      "values" = .parse_nd,
      "eligibilities" = .parse_nested_ragged,
      "associations" = .parse_nested_ragged
    )
  )
}

dnames_map <- function() {
  list(
    "ANCCR" = list(
      "ij_eligibilities" = c("s1"),
      "i_eligibilities" = c("s1"),
      "i_base_rate" = c("s1"),
      "ij_base_rate" = c("s1", "s2"),
      "representation_contingencies" = c("s1", "s2"),
      "net_contingencies" = c("s1", "s2"),
      "anccrs" = c("s1", "s2"),
      "causal_weights" = c("s1", "s2"),
      "dopamines" = c("s1", "s2"),
      "action_values" = c("s1", "s2"),
      "probabilities" = c("s1", "s2")
    ),
    "TD" = list(
      "associations" = c("s1", "s2", "t_bin", "value"),
      "eligibilities" = c("s1", "t_bin", "value"),
      "values" = c("s1", "t_bin")
    )
  )
}

plots_map <- function() {
  list(
    "ANCCR" = list(
      "ij_eligibilities" = plot_trials,
      "i_eligibilities" = plot_trials,
      "i_base_rate" = plot_trials,
      "ij_base_rate" = plot_targeted_trials,
      "representation_contingencies" = plot_targeted_typed_trials,
      "net_contingencies" = plot_targeted_trials,
      "anccrs" = plot_targeted_trials,
      "causal_weights" = plot_targeted_trials,
      "dopamines" = plot_targeted_trials,
      "action_values" = plot_targeted_trials,
      "probabilities" = plot_targeted_trials
    ),
    "TD" = list(
      "associations" = function(data, ...) {
        p <- plot_targeted_tbins(data, ...)
        # change x-label label
        t <- max(p$data$trial)
        p + ggplot2::labs(
          x = sprintf("Onset-relative Time Bin (Trial: %s)", t)
        )
      },
      "values" = plot_tbins,
      "eligibilities" = plot_tbins
    )
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
