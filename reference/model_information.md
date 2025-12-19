# Model information functions

An assortment of functions to return model information.

## Usage

``` r
supported_models()

supported_timed_models()

supported_optimizers()

supported_families()

supported_plots(model = NULL)

get_model(model)

model_parameters(model = NULL)

model_outputs(model = NULL)
```

## Arguments

- model:

  A string specifying a model. One from `supported_models()`.

## Value

`supported_models()` returns a character vector.

`supported_timed_models()` returns a character vector.

`supported_optimizers()` returns a character vector.

`supported_families()` returns a character vector.

`supported_plots()` returns a character vector or list (if model is
NULL).

`get_model()` returns a
[CalmrModel](https://victornavarro.org/calmr/reference/CalmrModel-class.md)
model instance.

`model_parameters()` returns a list or a list of lists (if model is
NULL).

`model_outputs()` returns a character vector or list (if model is NULL).

## Examples

``` r
# Outputs and plots supported by the RW1972 model
model_outputs("RW1972")
#> [1] "associations" "responses"   

# Getting a model instance of the PKH1982 model
pkh_inst <- get_model("PKH1982")

# Getting the `run` method for the MAC1975
head(methods::getMethod("run", "MAC1975"), 10)
#>                                                              
#> 1  new("MethodDefinition", .Data = function (object, ...)    
#> 2  {                                                         
#> 3      .local <- function (object, experience, mapping, ...) 
#> 4      {                                                     
#> 5          .assert_has_parameters(object)                    
#> 6          parameters <- object@parameters                   
#> 7          .assert_no_functional(mapping)                    
#> 8          .assert_no_functional(mapping)                    
#> 9          ntrials <- length(experience$tp)                  
#> 10         stim_names <- mapping$unique_nominal_stimuli      

# Getting the parameters required by SM2007
model_parameters("SM2007")
#> $name
#> [1] "alphas"  "lambdas" "omegas"  "rhos"    "gammas"  "taus"    "order"  
#> 
#> $default_value
#> [1] 0.4 1.0 0.2 1.0 1.0 0.2 1.0
#> 
#> $is_global
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
#> 
model_parameters("RW1972")
#> $name
#> [1] "alphas"    "betas_on"  "betas_off" "lambdas"  
#> 
#> $default_value
#> [1] 0.4 0.4 0.4 1.0
#> 
#> $is_global
#> [1] FALSE FALSE FALSE FALSE
#> 
```
