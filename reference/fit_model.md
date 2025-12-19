# Fit model to data

Obtain MLE estimates for model, given data.

## Usage

``` r
fit_model(data, model_function, optimizer_options, file = NULL, ...)
```

## Arguments

- data:

  A numeric vector containing data to fit model against.

- model_function:

  A function that runs the model and returns data.frame of value,
  organized as in `data`.

- optimizer_options:

  A list with options for the optimizer, as returned by
  [get_optimizer_opts](https://victornavarro.org/calmr/reference/get_optimizer_opts.md).

- file:

  A path to save the model fit. If the arguments to the fit call are
  found to be identical to those in the file, the model just gets
  loaded.

- ...:

  Extra parameters passed to the optimizer call.

## Value

A
[CalmrFit](https://victornavarro.org/calmr/reference/CalmrFit-class.md)
object

## Note

See the calmr_fits vignette for examples

## See also

[`get_optimizer_opts()`](https://victornavarro.org/calmr/reference/get_optimizer_opts.md)

## Examples

``` r
# Make some fake data
df <- data.frame(g = "g", p1 = "!3A>(US)")
pars <- get_parameters(df, model = "RW1972")
pars$alphas["US"] <- 0.9
exper <- make_experiment(df, parameters = pars, model = "RW1972")
res <- run_experiment(exper, outputs = "responses")
responses <- results(res)$responses$value

# define model function
model_fun <- function(p, ex) {
  np <- parameters(ex)
  np[[1]]$alphas[] <- p
  parameters(ex) <- np
  results(run_experiment(ex))$responses$value
}

# Get optimizer options
optim_opts <- get_optimizer_opts(
  model_pars = names(pars$alphas),
  ll = rep(.05, 2), ul = rep(.95, 2),
  optimizer = "optim", family = "identity"
)
optim_opts$initial_pars[] <- rep(.6, 2)

fit_model(responses, model_fun, optim_opts,
  ex = exper, method = "L-BFGS-B",
  control = list(maxit = 1)
)
#> Calmr model fit
#> --------------
#> Parameters:
#>         A        US 
#> 0.3893562 0.6000000 
#> --------------
#> 
#> nLogLik: 11.0273
```
