# CalmrFit methods

S4 methods for `CalmrFit` class.

## Usage

``` r
# S4 method for class 'CalmrFit'
show(object)

# S4 method for class 'CalmrFit'
predict(object, type = "response", ...)

# S4 method for class 'CalmrFit'
NLL(object)

# S4 method for class 'CalmrFit'
AIC(object, k = 2)

# S4 method for class 'CalmrFit'
BIC(object)
```

## Arguments

- object:

  A `CalmrFit` object.

- type:

  A string specifying the type of prediction to generate.

- ...:

  Extra named arguments.

- k:

  Penalty term for `AIC` method.

## Value

- `show()` returns NULL (invisibly).

- `predict()` returns a numeric vector.

- `NLL()` returns the negative log likelihood of the model.

- `AIC()` returns the Akaike Information Criterion (AIC) of the model.

- `BIC()` returns the Bayesian Information Criterion (BIC) of the model.

## Details

With `type = "response"`, the `predict()` function passed model
responses to the link function used to fit the model.

The AIC is defined as `2*k - 2*-NLL`, where k is a penalty term and NLL
is the negative log likelihood of the model.

The BIC is defined as `p*log(n) - 2*-NLL`, where p is the number of
parameters in the model and n is the number of observations
