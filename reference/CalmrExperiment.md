# S4 class for calmr experiments.

S4 class for calmr experiments.

## Slots

- `design`::

  A
  [CalmrDesign](https://victornavarro.org/calmr/reference/CalmrDesign-class.md)
  object.

- `groups`::

  A string specifying the groups in the design.

- `model`::

  A string specifying the model used.

- `parameters`::

  A list with the parameters used, per group.

- `timings`::

  A list with the timings used in the design.

- `experiences`::

  A list with the experiences for the model.

- `results`::

  A list with aggregated results.

- `models`::

  The models associated with the iteration.

- `.groups`::

  Internal. The groups associated with the iteration.

- `.iter`::

  Internal. The iteration number.

- `.seed`::

  The seed used to generate the experiment.

## See also

CalmrExperiment-methods
