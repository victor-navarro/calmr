# S4 class for calmr Fit

S4 class for calmr Fit

## Slots

- `nloglik`::

  Numeric. Negative log likelihood of the fit

- `best_pars`::

  Numeric. Best fitting parameters

- `model_pars`::

  Numeric. Parameters used in the model function

- `link_pars`::

  Numeric. Parameters used in the link function

- `data`::

  Numeric. Data used for fit

- `model_function`::

  Function. Model function

- `link_function`::

  Function. Link function

- `ll_function`::

  Function. Objective function (usually nloglikelihood)

- `optimizer_options`::

  List. Options used for the optimizer

- `extra_pars`::

  List. Extra parameters passed to the fit call (...)

## See also

CalmrFit-methods
