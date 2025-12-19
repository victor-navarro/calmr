# CalmrRSA methods

S4 methods for `CalmrRSA` class.

## Usage

``` r
# S4 method for class 'CalmrRSA'
show(object)

# S4 method for class 'CalmrRSA'
test(object, n_samples = 1000, p = 0.95)

# S4 method for class 'CalmrRSA'
plot(x)
```

## Arguments

- object, x:

  A `CalmrRSA` object.

- n_samples:

  The number of samples for the permutation test (default = 1e3)

- p:

  The critical threshold level for the permutation test (default = 0.95)

## Value

- `show()` returns NULL (invisibly).

- `test()` returns the `CalmrRSA` object with permutation test data.

- [`plot()`](https://victornavarro.org/calmr/reference/CalmrExperiment-methods.md)
  returns a list of 'ggplot' plot objects.
