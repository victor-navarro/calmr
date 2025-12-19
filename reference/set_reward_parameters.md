# Set reward parameters for ANCCR model

Set reward parameters for ANCCR model

## Usage

``` r
set_reward_parameters(parameters, rewards = c("US"))
```

## Arguments

- parameters:

  A list of parameters, as returned by
  [`get_parameters()`](https://victornavarro.org/calmr/reference/get_parameters.md)

- rewards:

  A character vector specifying the reward stimuli. Default = `c("US")`

## Value

A list of parameters

## Note

The default behaviour of `get_parameters` for the ANCCR model is to set
every reward-related parameter to its non-zero default value. This
function will set those parameters to zero for non-reward stimuli
