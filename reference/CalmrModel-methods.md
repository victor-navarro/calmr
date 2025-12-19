# CalmrModel methods

S4 methods for
[CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)

## Usage

``` r
# S4 method for class 'CalmrModel'
run(object, experience, mapping, timings, ...)

# S4 method for class 'CalmrModel'
parameters(x)

# S4 method for class 'CalmrModel'
parameters(x) <- value

# S4 method for class 'CalmrModel'
raw_results(object)

# S4 method for class 'CalmrModel'
parsed_results(object)

# S4 method for class 'CalmrModel'
show(object)

# S4 method for class 'CalmrModel'
parse(object, outputs = object@outputs)

# S4 method for class 'CalmrModel'
plot(x, type = NULL, ...)

# S4 method for class 'CalmrModel'
graph(x, ...)

# S4 method for class 'ANCCR'
run(object, experience, mapping, timings, ..., debug = FALSE, debug_t = -1)

# S4 method for class 'HDI2020'
run(object, experience, mapping, ...)

# S4 method for class 'HD2022'
run(object, experience, mapping, ...)

# S4 method for class 'MAC1975'
run(object, experience, mapping, ...)

# S4 method for class 'PKH1982'
run(object, experience, mapping, ...)

# S4 method for class 'RAND'
run(object, experience, mapping, ...)

# S4 method for class 'RW1972'
run(object, experience, mapping, ...)

# S4 method for class 'SM2007'
run(
  object,
  experience,
  mapping,
  debug = FALSE,
  comparator_func = .witnauer_comparator_proc,
  ...
)

# S4 method for class 'TD'
run(object, experience, mapping, timings, ...)
```

## Arguments

- object:

  A
  [CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
  object.

- experience:

  A data.frame specifying trials as rows, as returned by
  [`make_experiment()`](https://victornavarro.org/calmr/reference/make_experiment.md).

- mapping:

  A named list specifying trial and stimulus mapping, as returned by
  [`make_experiment()`](https://victornavarro.org/calmr/reference/make_experiment.md).

- timings:

  A named list specifying timings for the model. Only used for timed
  models.

- ...:

  Additional named arguments.

- x:

  A
  [CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
  object.

- value:

  A list of parameters to set.

- outputs:

  A character vector specifying the outputs to parse. If not specified,
  all outputs of the model will be parsed.

- type:

  A character vector specifying the types of plots to generate (should
  be model outputs).

- debug:

  A logical to print debugging messages.

- debug_t:

  A trial to debug at.

- comparator_func:

  The function for the comparator process.

## Value

`run()` returns the
[CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
after running the phases in the design.

[`parameters()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
returns the parameters of the
[CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
object.

`parameters()<-` sets the parameters of a
[CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
object.

[`raw_results()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
returns the last raw results of the
[CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
object.

[`parsed_results()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
returns the last parsed results of the
[CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
object.

`show()` returns NULL (invisibly).

[`parse()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
returns
[CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
with parsed results.

[`plot()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
returns a list of 'ggplot' plot objects.

[`graph()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
returns a 'ggplot' object.

## Note

The `run` method changes some internal states of the model (if
appropriate) and populates the `.last_raw_results` slot with the results
of the run.
