# Run models given a set of parameters

Run models given a set of parameters

## Usage

``` r
compare_models(x, models = NULL, ...)
```

## Arguments

- x:

  A list of
  [CalmrExperiment](https://victornavarro.org/calmr/reference/CalmrExperiment.md)
  objects or a design
  [data.frame](https://rdrr.io/r/base/data.frame.html).

- models:

  A character vector of length m, specifying the models to run. Ignored
  if x is a list of
  [CalmrExperiment](https://victornavarro.org/calmr/reference/CalmrExperiment.md)
  objects.

- ...:

  Arguments passed to
  [make_experiment](https://victornavarro.org/calmr/reference/make_experiment.md).

## Value

A list of
[CalmrExperiment](https://victornavarro.org/calmr/reference/CalmrExperiment.md)
objects

## Examples

``` r
# By making experiment beforehand (recommended)
df <- get_design("blocking")
models <- c("HD2022", "RW1972", "PKH1982")
exps <- lapply(models, function(m) {
  make_experiment(df,
    parameters = get_parameters(df, model = m),
    model = m
  )
})
comp <- compare_models(exps)

# By passing minimal arguments (not recommended; default parameters)
comp <- compare_models(df, models = models)
```
