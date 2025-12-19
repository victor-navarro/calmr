# Parse design data.frame

Parse design data.frame

## Usage

``` r
parse_design(df)
```

## Arguments

- df:

  A `data.frame` of dimensions (groups) by (phases+1).

## Value

A
[CalmrDesign](https://victornavarro.org/calmr/reference/CalmrDesign-class.md)
object.

## Note

Each entry in even-numbered columns of df is a string formatted as per
[`phase_parser()`](https://victornavarro.org/calmr/reference/phase_parser.md).

## See also

[`phase_parser()`](https://victornavarro.org/calmr/reference/phase_parser.md)

## Examples

``` r
df <- data.frame(
  Group = c("Group 1", "Group 2"),
  P1 = c("10AB(US)", "10A(US)")
)
parse_design(df)
#> CalmrDesign built from data.frame:
#>     Group       P1
#> 1 Group 1 10AB(US)
#> 2 Group 2  10A(US)
#> ----------------
#> Trials detected:
#>     group phase trial_names trial_repeats is_test stimuli
#> 1 Group 1    P1      AB(US)            10   FALSE  A;B;US
#> 2 Group 2    P1       A(US)            10   FALSE    A;US
```
