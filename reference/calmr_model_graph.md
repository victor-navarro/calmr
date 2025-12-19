# Create a graph with calmr data

`patch_graphs()` patches graphs with 'patchwork'

## Usage

``` r
calmr_model_graph(
  x,
  loops = TRUE,
  limits = max(abs(x$value)) * c(-1, 1),
  colour_key = FALSE,
  t = max(x$trial),
  options = get_graph_opts()
)

patch_graphs(graphs, selection = names(graphs))

get_graph_opts(graph_size = "small")
```

## Arguments

- x:

  A `data.frame`-like with data to use in the plot. Contains a column
  named `value`.

- loops:

  Logical. Whether to draw arrows back and forth

- limits:

  Numerical. Limits for color scale. Defaults to
  max(abs(x\$value))\*c(-1,1).

- colour_key:

  Logical. Whether to show the color key

- t:

  The trial from which weights are obtained (defaults to the maximum
  trial in the data).

- options:

  A list with graph options, as returned by `get_graph_opts()`.

- graphs:

  A list of (named) graphs, as returned by
  [`graph()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
  or `calmr_model_graph()`

- selection:

  A character or numeric vector determining the plots to patch.

- graph_size:

  A string (either "small" or "large"). to return default values for
  small or large graphs

- trial:

  Numerical. The trial to graph.

## Value

A 'ggplot' object

`patch_graphs()` returns a 'patchwork' object

A list with graph options, to be passed to
[`ggnetwork::geom_nodes()`](https://rdrr.io/pkg/ggnetwork/man/geom_nodes.html).

## Note

You should probably be getting graphs via the graph method for
[CalmrExperiment](https://victornavarro.org/calmr/reference/CalmrExperiment.md).
