# Get model parameters

Get model parameters

## Usage

``` r
get_parameters(design, model)
```

## Arguments

- design:

  A `data.frame` containing the experimental design.

- model:

  A string specifying a model. One in
  [`supported_models()`](https://victornavarro.org/calmr/reference/model_information.md).

## Value

A list with model parameters depending on model

## Examples

``` r
block <- get_design("blocking")
get_parameters(block, model = "SM2007")
#> $alphas
#>   L   N  US 
#> 0.4 0.4 0.4 
#> 
#> $lambdas
#>  L  N US 
#>  1  1  1 
#> 
#> $omegas
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $rhos
#>  L  N US 
#>  1  1  1 
#> 
#> $gammas
#>  L  N US 
#>  1  1  1 
#> 
#> $taus
#>   L   N  US 
#> 0.2 0.2 0.2 
#> 
#> $order
#> [1] 1
#> 
```
