# CalmrExperiment methods

S4 methods for `CalmrExperiment` class.

## Usage

``` r
# S4 method for class 'CalmrExperiment'
show(object)

# S4 method for class 'CalmrExperiment'
design(x)

# S4 method for class 'CalmrExperiment'
trials(object)

# S4 method for class 'CalmrExperiment'
parameters(x)

# S4 method for class 'CalmrExperiment'
parameters(x) <- value

# S4 method for class 'CalmrExperiment'
experiences(x)

# S4 method for class 'CalmrExperiment'
experiences(x) <- value

# S4 method for class 'CalmrExperiment'
results(object)

# S4 method for class 'CalmrExperiment'
raw_results(object)

# S4 method for class 'CalmrExperiment'
parsed_results(object)

# S4 method for class 'CalmrExperiment'
length(x)

# S4 method for class 'CalmrExperiment'
parse(object, outputs = NULL)

# S4 method for class 'CalmrExperiment'
aggregate(x, outputs = NULL)

# S4 method for class 'CalmrExperiment'
plot(x, type = NULL, ...)

# S4 method for class 'CalmrExperiment'
graph(x, ...)

# S4 method for class 'CalmrExperiment'
timings(x)

# S4 method for class 'CalmrExperiment'
timings(x) <- value

# S4 method for class 'CalmrExperiment'
filter(x, trial_types = NULL, phases = NULL, stimuli = NULL)
```

## Arguments

- object, x:

  A `CalmrExperiment` object.

- value:

  A list of parameters (or list of parameter lists).

- outputs:

  A character vector specifying the model outputs to parse.

- type:

  A character vector specifying the type(s) of plots to create. Defaults
  to NULL. See
  [supported_plots](https://victornavarro.org/calmr/reference/model_information.md).

- ...:

  Extra arguments passed to
  [`calmr_model_graph()`](https://victornavarro.org/calmr/reference/calmr_model_graph.md).

- trial_types:

  A character vector with trial types to filter.

- phases:

  A character vector with phase names to filter.

- stimuli:

  A character vector with stimulus names to filter.

## Value

`show()` returns NULL (invisibly).

`design()` returns the `CalmrDesign` contained in the object.

`trials()` returns NULL (invisibly).

`parameters()` returns the list of parameters contained in the object.

`parameters()<-` returns the object after updating parameters.

`experiences()` returns a list of `data.frame` objects containing model
training routines.

`experiences()<-` returns the object after updating experiences.

`results()` returns a `data.table` objects with aggregated results.

`raw_results()` returns a list with raw model results.

`parsed_results()` returns a list of `data.table` objects with parsed
results.

[`length()`](https://rdrr.io/r/base/length.html) returns an integer
specifying the total length of the experiment (groups by iterations).

`parse()` returns the object after parsing raw results.

`aggregate()` returns the object after aggregating parsed results.

`plot()` returns a list of 'ggplot' plot objects.

`graph()` returns a list of 'ggplot' plot objects.

`timings()` returns the list of timings contained in the object.

`timings()<-` returns the object after updating timings.

`filter()` returns the object after filtering parsed aggregated results
