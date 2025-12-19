# Get timing design parameters

Get timing design parameters

## Usage

``` r
get_timings(design, model)
```

## Arguments

- design:

  A `data.frame` containing the experimental design.

- model:

  One of
  [`supported_timed_models()`](https://victornavarro.org/calmr/reference/model_information.md).

## Value

A list of timing design parameters.

## Examples

``` r
block <- get_design("blocking")
get_timings(block, model = "TD")
#> $use_exponential
#> [1] TRUE
#> 
#> $time_resolution
#> [1] 0.5
#> 
#> $sample_timings
#> [1] TRUE
#> 
#> $trial_ts
#>     trial post_trial_delay mean_ITI max_ITI
#> 1  N>(US)                1       30      90
#> 2 NL>(US)                1       30      90
#> 3      #L                1       30      90
#> 
#> $period_ts
#>     trial period stimulus stimulus_duration
#> 1  N>(US)      N        N                 1
#> 2  N>(US)   (US)       US                 1
#> 3 NL>(US)     NL        N                 1
#> 4 NL>(US)     NL        L                 1
#> 5 NL>(US)   (US)       US                 1
#> 6      #L      L        L                 1
#> 
#> $transition_ts
#>     trial transition transition_delay
#> 1  N>(US)     N>(US)                1
#> 2 NL>(US)    NL>(US)                1
#> 
```
