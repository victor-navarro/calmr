# Get optimizer options

Get optimizer options

## Usage

``` r
get_optimizer_opts(
  model_pars,
  initial_pars = rep(NA, length(model_pars)),
  ll = rep(NA, length(model_pars)),
  ul = rep(NA, length(model_pars)),
  optimizer = NULL,
  family = NULL
)
```

## Arguments

- model_pars:

  A character vector specifying the name of the parameters to fit.

- initial_pars:

  A numeric vector specifying the initial parameter values to \#'
  evaluate the model at (required by `optim`). Defaults to 0 for each
  parameter.

- ll, ul:

  A numeric vector specifying the lower and upper limits of the
  parameters to fit, respectively

- optimizer:

  A string specifying the optimizer to use. One from `c("optim", "ga")`

- family:

  A string specifying the family function to generate responses (and
  calculate the likelihood function with). One from
  `c("identity", "normal", "poisson")`.

## Value

A list with optimizer options.

## Note

Whenever a family function other than the identity is used, the
family-specific parameters will always be appended to the end of the
relevant lists.

## See also

[`fit_model()`](https://victornavarro.org/calmr/reference/fit_model.md)
