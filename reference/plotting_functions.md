# General plotting functions

`plot_targeted_tbins()` plots targeted time data on a trial.

`plot_tbins()` plots non-targeted time data on a trial.

`plot_targeted_trials()` plots targeted trial data.

`plot_trials()` plots non-targeted trial data.

`plot_targeted_typed_trials()` plots targeted trial data with a type.

`plot_targeted_complex_trials()` plots targeted data with a third
variable.

## Usage

``` r
plot_targeted_tbins(data, t = max(data$trial))

plot_tbins(data, t = max(data$trial))

plot_targeted_trials(data)

plot_trials(data)

plot_targeted_typed_trials(data)

plot_targeted_complex_trials(data, col)
```

## Arguments

- data:

  A `data.frame`-like with data to plot.

- t:

  A numeric vector specifying the trial(s) to plot. Defaults to the last
  trial in data.

- col:

  A string specifying the column of the third variable.

## Value

`plot_targeted_tbins()` returns 'ggplot' object.

`plot_tbins()` returns 'ggplot' object.

`plot_targeted_trials()` returns 'ggplot' object.

`plot_trials()` returns 'ggplot' object.

`plot_targeted_typed_trials()` returns 'ggplot' object.

`plot_targeted_complex_trials()` returns 'ggplot' object.

## Note

These functions are not meant to be used by non-developers. If you want
to plot data from a model or an experiment, see the
[`plot()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
method. All data must be parsed or aggregated, as returned by
[`results()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
or
[`parsed_results()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md).
