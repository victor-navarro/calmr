# Parses a phase string

Parses a phase string

## Usage

``` r
phase_parser(phase_string)
```

## Arguments

- phase_string:

  A string specifying trials within a phase.

## Value

A named list with:

- trial_info::

  A trial-named list of lists.

- general_info::

  General phase information.

## Note

This function is meant for internal use only, but we expose it so you
can test your strings.

## See also

[`parse_design()`](https://victornavarro.org/calmr/reference/parse_design.md)

## Examples

``` r
# A silly (but valid) string
phase_parser("10#Rescorla>Wagner")
#> $trial_info
#> $trial_info$`10#Rescorla>Wagner`
#> $trial_info$`10#Rescorla>Wagner`$name
#> [1] "#Rescorla>Wagner"
#> 
#> $trial_info$`10#Rescorla>Wagner`$repetitions
#> [1] 10
#> 
#> $trial_info$`10#Rescorla>Wagner`$is_test
#> [1] TRUE
#> 
#> $trial_info$`10#Rescorla>Wagner`$periods
#> [1] "Rescorla" "Wagner"  
#> 
#> $trial_info$`10#Rescorla>Wagner`$nominals
#> $trial_info$`10#Rescorla>Wagner`$nominals$Rescorla
#> [1] "R" "e" "s" "c" "o" "r" "l" "a"
#> 
#> $trial_info$`10#Rescorla>Wagner`$nominals$Wagner
#> [1] "W" "a" "g" "n" "e" "r"
#> 
#> 
#> $trial_info$`10#Rescorla>Wagner`$functionals
#> $trial_info$`10#Rescorla>Wagner`$functionals$Rescorla
#> [1] "R" "e" "s" "c" "o" "r" "l" "a"
#> 
#> $trial_info$`10#Rescorla>Wagner`$functionals$Wagner
#> [1] "W" "a" "g" "n" "e" "r"
#> 
#> 
#> $trial_info$`10#Rescorla>Wagner`$all_nominals
#>  [1] "R" "e" "s" "c" "o" "r" "l" "a" "W" "a" "g" "n" "e" "r"
#> 
#> $trial_info$`10#Rescorla>Wagner`$all_functionals
#>  [1] "R" "e" "s" "c" "o" "r" "l" "a" "W" "a" "g" "n" "e" "r"
#> 
#> 
#> 
#> $general_info
#> $general_info$trial_names
#> [1] "#Rescorla>Wagner"
#> 
#> $general_info$trial_repeats
#> [1] 10
#> 
#> $general_info$is_test
#> [1] TRUE
#> 
#> $general_info$randomize
#> [1] FALSE
#> 
#> $general_info$nomi2func
#>   R   e   s   c   o   r   l   a   W   g   n 
#> "R" "e" "s" "c" "o" "r" "l" "a" "W" "g" "n" 
#> 
#> $general_info$func2nomi
#>   R   e   s   c   o   r   l   a   W   g   n 
#> "R" "e" "s" "c" "o" "r" "l" "a" "W" "g" "n" 
#> 
#> 

# An invalid string that needs trial repetitions for one of trials.
try(phase_parser("10#Rescorla/Wagner"))
#> Error in if (is.na(treps)) 1 else treps : argument is of length zero
```
