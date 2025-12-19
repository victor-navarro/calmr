# General plotting options

`plot_common_scale()` rescales a list of plots to have a common scale.

`get_plot_opts()` returns generic plotting options.

`patch_plots()` patches plots using `patchwork` package.

## Usage

``` r
plot_common_scale(plots)

get_plot_opts(common_scale = TRUE)

patch_plots(plots, selection = names(plots), plot_options = get_plot_opts())
```

## Arguments

- plots:

  A list of (named) plots, as returned by
  [`plot()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md).

- common_scale:

  Logical specifying whether to have plots in a common scale.

- selection:

  A character or numeric vector determining the plots to patch

- plot_options:

  A list of plot options as returned by `get_plot_opts()`

## Value

`plot_common_scale()` returns a list of plots.

`get_plot_opts()` returns a list.

`patch_plots()` returns a `patchwork` object.
