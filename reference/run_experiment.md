# Run experiment

Runs an experiment with minimal parameters.

## Usage

``` r
run_experiment(x, outputs = NULL, parse = TRUE, aggregate = TRUE, ...)
```

## Arguments

- x:

  A
  [CalmrExperiment](https://victornavarro.org/calmr/reference/CalmrExperiment.md)
  or design `data.frame`

- outputs:

  A character vector specifying which outputs to parse and aggregate.
  Defaults to NULL, in which case all model outputs are
  parsed/aggregated.

- parse:

  A logical specifying whether the raw results should be parsed. Default
  = TRUE.

- aggregate:

  A logical specifying whether the parsed results should be aggregated.
  Default = TRUE.

- ...:

  Arguments passed to other functions

## Value

A
[CalmrExperiment](https://victornavarro.org/calmr/reference/CalmrExperiment.md)
with results.

## Examples

``` r
# Using a data.frame only (throws warning)
df <- get_design("relative_validity")
run_experiment(df, model = "RW1972")
#> Warning: Using default model parameters.
#> -----------------------------
#> CalmrExperiment with model:
#> character
#> -----------------------------
#> Design:
#>    Group                       P1  P2
#> 1   True           !10AB(US)/10AC 1#A
#> 2 Pseudo !5AB(US)/5AB/5AC(US)/5AC 1#A
#> -----------------------------
#> Parameters:
#> $True
#> $True$alphas
#>   A   B   C  US 
#> 0.4 0.4 0.4 0.4 
#> 
#> $True$betas_on
#>   A   B   C  US 
#> 0.4 0.4 0.4 0.4 
#> 
#> $True$betas_off
#>   A   B   C  US 
#> 0.4 0.4 0.4 0.4 
#> 
#> $True$lambdas
#>  A  B  C US 
#>  1  1  1  1 
#> 
#> 
#> $Pseudo
#> $Pseudo$alphas
#>   A   B   C  US 
#> 0.4 0.4 0.4 0.4 
#> 
#> $Pseudo$betas_on
#>   A   B   C  US 
#> 0.4 0.4 0.4 0.4 
#> 
#> $Pseudo$betas_off
#>   A   B   C  US 
#> 0.4 0.4 0.4 0.4 
#> 
#> $Pseudo$lambdas
#>  A  B  C US 
#>  1  1  1  1 
#> 

# Using custom parameters
df <- get_design("relative_validity")
pars <- get_parameters(df, model = "HD2022")
pars$alphas["US"] <- 0.6
run_experiment(df, parameters = pars, model = "HD2022")
#> -----------------------------
#> CalmrExperiment with model:
#> character
#> -----------------------------
#> Design:
#>    Group                       P1  P2
#> 1   True           !10AB(US)/10AC 1#A
#> 2 Pseudo !5AB(US)/5AB/5AC(US)/5AC 1#A
#> -----------------------------
#> Parameters:
#> $True
#> $True$alphas
#>   A   B   C  US 
#> 0.4 0.4 0.4 0.6 
#> 
#> 
#> $Pseudo
#> $Pseudo$alphas
#>   A   B   C  US 
#> 0.4 0.4 0.4 0.6 
#> 

# Using make_experiment, for more iterations
df <- get_design("blocking")
pars <- get_parameters(df, model = "SM2007")
exper <- make_experiment(df,
  parameters = pars, model = "SM2007",
  iterations = 4
)
run_experiment(exper)
#> -----------------------------
#> CalmrExperiment with model:
#> character
#> -----------------------------
#> Design:
#>      Group       P1             P2
#> 1 Blocking 10N>(US) 10NL>(US)/10#L
#> 2  Control          10NL>(US)/10#L
#> -----------------------------
#> Parameters:
#> $Blocking
#> $Blocking$alphas
#>   L   N  US 
#> 0.4 0.4 0.4 
#> 
#> $Blocking$lambdas
#>  L  N US 
#>  1  1  1 
#> 
#> $Blocking$omegas
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $Blocking$rhos
#>  L  N US 
#>  1  1  1 
#> 
#> $Blocking$gammas
#>  L  N US 
#>  1  1  1 
#> 
#> $Blocking$taus
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $Blocking$order
#> [1] 1
#> 
#> 
#> $Control
#> $Control$alphas
#>   L   N  US 
#> 0.4 0.4 0.4 
#> 
#> $Control$lambdas
#>  L  N US 
#>  1  1  1 
#> 
#> $Control$omegas
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $Control$rhos
#>  L  N US 
#>  1  1  1 
#> 
#> $Control$gammas
#>  L  N US 
#>  1  1  1 
#> 
#> $Control$taus
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $Control$order
#> [1] 1
#> 

# Only parsing the associations in the model, without aggregation
run_experiment(exper, outputs = "associations", aggregate = FALSE)
#> -----------------------------
#> CalmrExperiment with model:
#> character
#> -----------------------------
#> Design:
#>      Group       P1             P2
#> 1 Blocking 10N>(US) 10NL>(US)/10#L
#> 2  Control          10NL>(US)/10#L
#> -----------------------------
#> Parameters:
#> $Blocking
#> $Blocking$alphas
#>   L   N  US 
#> 0.4 0.4 0.4 
#> 
#> $Blocking$lambdas
#>  L  N US 
#>  1  1  1 
#> 
#> $Blocking$omegas
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $Blocking$rhos
#>  L  N US 
#>  1  1  1 
#> 
#> $Blocking$gammas
#>  L  N US 
#>  1  1  1 
#> 
#> $Blocking$taus
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $Blocking$order
#> [1] 1
#> 
#> 
#> $Control
#> $Control$alphas
#>   L   N  US 
#> 0.4 0.4 0.4 
#> 
#> $Control$lambdas
#>  L  N US 
#>  1  1  1 
#> 
#> $Control$omegas
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $Control$rhos
#>  L  N US 
#>  1  1  1 
#> 
#> $Control$gammas
#>  L  N US 
#>  1  1  1 
#> 
#> $Control$taus
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $Control$order
#> [1] 1
#> 
```
