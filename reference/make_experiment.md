# Make CalmrExperiment

Makes a `CalmrExperiment` object containing the arguments necessary to
run an experiment.

## Usage

``` r
make_experiment(
  design,
  model,
  parameters = NULL,
  timings = NULL,
  iterations = 1,
  miniblocks = TRUE,
  seed = NULL,
  .callback_fn = NULL,
  ...
)
```

## Arguments

- design:

  A design `data.frame`.

- model:

  A string specifying the model name. One of
  [`supported_models()`](https://victornavarro.org/calmr/reference/model_information.md).

- parameters:

  Optional. Parameters for a model as returned by
  [`get_parameters()`](https://victornavarro.org/calmr/reference/get_parameters.md).

- timings:

  Optional. Timings for a time-based design as returned by
  [`get_timings()`](https://victornavarro.org/calmr/reference/get_timings.md)

- iterations:

  An integer specifying the number of iterations per group. Default = 1.

- miniblocks:

  Whether to organize trials in miniblocks. Default = TRUE.

- seed:

  A valid seed for the RNG to make the experiment. Default = NULL, in
  which case the current RNG is used.

- .callback_fn:

  A function for keeping track of progress. Internal use.

- ...:

  Extra parameters passed to other functions.

## Value

A
[CalmrExperiment](https://victornavarro.org/calmr/reference/CalmrExperiment.md)
object.

## Note

The miniblocks option will direct the sampling function to create
equally-sized miniblocks with random trials within a phase. For example,
the phase string "2A/2B" will create two miniblocks with one of each
trial. The phase string "2A/4B" will create two miniblocks with one A
trial, and 2 B trials. However, the phase string "2A/1B" will not result
in miniblocks, even if miniblocks here is set to TRUE.

## See also

[`parse_design()`](https://victornavarro.org/calmr/reference/parse_design.md),

## Examples

``` r
des <- data.frame(Group = "G1", P1 = "10A>(US)")
ps <- get_parameters(des, model = "HD2022")
make_experiment(
  design = des, parameters = ps,
  model = "HD2022", iterations = 2
)
#> -----------------------------
#> CalmrExperiment with model:
#> character
#> -----------------------------
#> Design:
#>   Group       P1
#> 1    G1 10A>(US)
#> -----------------------------
#> Parameters:
#> $G1
#> $G1$alphas
#>   A  US 
#> 0.4 0.4 
#> 
```
