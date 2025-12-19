# Perform representational similarity analysis

Perform representational similarity analysis

## Usage

``` r
rsa(x, comparisons, test = FALSE, ...)
```

## Arguments

- x:

  A list of
  [CalmrExperiment](https://victornavarro.org/calmr/reference/CalmrExperiment.md)
  objects

- comparisons:

  A model-named list containing the model outputs to compare.

- test:

  Whether to test the RSA via permutation test. Default = FALSE.

- ...:

  Additional parameters passed to
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html) and
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html)

## Value

A CalmrRSA object

## Note

The object returned by this function can be later tested via its own
[`test()`](https://victornavarro.org/calmr/reference/CalmrRSA-methods.md)
method.

## Examples

``` r
# Comparing the associations in three models
exp <- data.frame(
  Group = c("A", "B"),
  P1 = c("!2(A)>(US)/1B>(US)", "!1(A)>(US)/2B>(US)")
)
models <- c("HD2022", "RW1972", "PKH1982")
parameters <- sapply(models, get_parameters, design = exp)
exp_res <- compare_models(exp,
  models = models
)
comparisons <- list(
  "HD2022" = c("associations"),
  "RW1972" = c("associations"),
  "PKH1982" = c("associations")
)
res <- rsa(exp_res, comparisons = comparisons)
test(res, n_samples = 20)
#> CalmrRSA object
#> ---------------
#> Correlation matrix:
#>                      HD2022.associations RW1972.associations
#> HD2022.associations            1.0000000          -0.9029598
#> RW1972.associations           -0.9029598           1.0000000
#> PKH1982.associations          -0.5632734           0.1535441
#>                      PKH1982.associations
#> HD2022.associations            -0.5632734
#> RW1972.associations             0.1535441
#> PKH1982.associations            1.0000000
#> ---------------
#> Significance matrix:
#>                      HD2022.associations RW1972.associations
#> HD2022.associations                FALSE               FALSE
#> RW1972.associations                FALSE               FALSE
#> PKH1982.associations               FALSE               FALSE
#>                      PKH1982.associations
#> HD2022.associations                 FALSE
#> RW1972.associations                 FALSE
#> PKH1982.associations                FALSE
#> From 20 permutation samples, two-tailed test with alpha = 0.05.
```
