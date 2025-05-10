# calmr 0.7.0
* Corrected some issues in directional models.
* Created a vignette to expose the behaviour of directional models.
* Removed randomization column requirement from designs. Randomization of phases is now specified using the "!" character anywhere in the phase string. Using the old format throws a deprecation warning.
* Added support for seed experiment generation in `make_experiment()`.
* Added `set_calmr_palette()` function to control the colour/fill scales used to plot results ([#1](https://github.com/victor-navarro/calmr/issues/1)).
* Added `filter()` method for `CalmrExperiment` class that allows filtering of aggregated data ([#1](https://github.com/victor-navarro/calmr/issues/1)).
* Fixed bug in `make_experiment()` that was triggered by empty phases and no miniblocks.
* Changed `get_timings()` to require a specific model name.
* Added vignette for TD model.

# calmr 0.6.2
* Aggregation of ANCCR data now ignores time; time entries are averaged.
* Added the Temporal Difference model under the name "TD". The model is in an experimental state.
* Experiments for time-based models now require a separate list to construct time-based experiences. See `get_timings()`.
* Added `experiences<-`, `timings`, `timings<-` methods for `CalmrExperiment` class.
* Revamped plotting functions and parsing functions.
* Revamped output names for all models to make them more intelligible.
* Fixed a bug related to the aggregation of pools in HDI2020 and HD2022.
* Consolidated some man pages.

# calmr 0.6.1
* Added `outputs` argument to `run_experiment()`, `parse()`, and `aggregate()`, allowing the user to parse/aggregate only some model outputs.
* Documentation corrections for CRAN resubmission.

# calmr 0.6.0
* Added dependency on `data.table` resulting in great speedups for large experiments.
* Replaced dependency on `cowplot` with dependency on `patchwork`.
* Removed dependencies on `tibble`, `dplyr`, `tidyr`, and other packages from the `tidyverse`.
* Removed `shiny` app from the package.
* The previous app is now distributed separately via the `calmr.app` package available on GitHub.
* Test coverage has reached 100%.
* The package is now ready for CRAN submission.

# calmr 0.5.1
* Added parallelization and progress bars via `future`, `future.apply`, and `progressr`.
* Function `calmr_verbosity` can set the verbosity of the package.

# calmr 0.5.0
* Implementation of ANCCR (Jeong et al., 2022), the first time-based model included in `calmr`.
* Added parameter distinction between trial-wise and period-wise parameters.
* Added internal augmentation of arguments depending on the model.
* All trial-based models do not use pre/post distinctions anymore. Using the ">" special character does not affect these models anymore.
* The ">" special character is used to specify periods within a trial. For example, "A>B>C" implies A is followed by B which is followed by C. See the `using_time_models` vignette for additional information.
* Named stimuli now support numbers trailing characters (e.g., "(US1)" is valid now.)

# calmr 0.4.0
* Major refactoring of classes and models. This should help development moving forward.
* Added several methods for access to CalmrExperiment contents, including `c` (to bind experiments) `results`, `plot`, `graph`, `design`, and `parameters`.
* Created CalmrDesign and CalmrResult classes. 
* Rewrote parsers to be less verbose and to rely less on the `tidyverse` suite and piping.
* Substantially reduced the complexity of `make_experiment` function (previous `make_experiment`).
* Introduced distinction between stimulus-specific and global parameters.
* Parameters are now lists instead of data.frames.
* Modified UI for calmr app to include a sidebar. 
* Simplified the app by removing some of the options.
* Nearly duplicated the number of tests.

# calmr 0.3.0

* Added first version of the SOCR model (SM2007) as well as two vignettes explaining the math behind the implementation and some quick simulations.
* Documentation progress.

# calmr 0.2.0

* Added multiple models to package and app (RW1972, PKH1982, MAC1975).
* Implementation of basic S4 classes for model, experiment, fit, and RSA comparison objects, as well as their methods.
* Added genetic algorithms (via `GA`) for parameter estimation.
* Added basic tools to perform representational similarity analysis.
* Documentation progress.

# calmr 0.1.0

* `heidi` is now `calmr`. The package now aims to maintain several associative learning models and implement tools for their use.
* Major overhaul of the training function (train_pav_model). All relevant calculations are now done as a function of all functional stimuli instead of just the US.
* Support for the specification of expectation/correction steps within the trial via ">". For example, the trial "A>(US)" will use only A to generate the expectation, but will learn about both stimuli during the correction step.
* The previous plotting function for R-values has been revamped to allow both simple and complex versions. The complex version facets r-values on a predictor basis, and uses colour lines for each target.
* Bugfix related to stimulus saliencies.
