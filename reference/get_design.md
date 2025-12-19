# Get basic designs

Get basic designs

## Usage

``` r
get_design(design_name = NULL)
```

## Arguments

- design_name:

  A string specifying a design name (default = NULL)

## Value

If design_name is not NULL, a data.frame containing the design.
Otherwise, a list containing all available designs.

## See also

[`parse_design()`](https://victornavarro.org/calmr/reference/parse_design.md)

## Examples

``` r
names(get_design())
#> [1] "blocking"            "relative_validity"   "controlled_blocking"
get_design("blocking")
#>      Group       P1             P2
#> 1 Blocking 10N>(US) 10NL>(US)/10#L
#> 2  Control          10NL>(US)/10#L
```
