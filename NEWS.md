# calmr 0.5.1
* Added parallelization and progress bars via `future`, `future.apply`, and `progressr`.
* Function `calmr_verbosity` can set the verbosity of the package.

# calmr 0.5.0
* Implementation of ANCCR (Jeong et al., 2022), the first time-based model included in calmr.
* Added parameter distinction between trial-wise and period-wise parameters.
* Added internal augmentation of design/arguments depending on the model.
* All trial-based models do not use pre/post distinctions anymore. Using the ">" special character does not affect these models anymore.
* The ">" special character is used to specify periods within a trial. For example, "A>B>C" implies A is followed by B which is followed by C. See "using_time_models" vignette for additional information.
* Named stimuli now support numbers trailing characters (e.g., "(US1)" is valid now.)


# calmr 0.4.0
* Major refactoring of classes and models. This should help development moving forward.
* Added several methods for access to CalmrExperiment contents, including `c` (to bind experiments) `results`, `plot`, `graph`, `design` and `parameters`.
* Created CalmrDesign and CalmrResult classes. 
* Rewrote parsers to be less verbose and to rely less on the tidyverse suite and piping.
* Substantially reduced the complexity of `make_experiment` function (previous `make_experiment`).
* Introduced distinction between stimulus-specific and global parameters.
* Parameters are now lists instead of data.frames.
* Modified UI for calmr app to include a sidebar (to reduce clutter). 
* Simplified the app by removing some of the options.
* Nearly duplicated the number of tests.

# calmr 0.3.0

* Added first version of the SOCR model (SM2007) as well as two vignettes explaining the math behind the implementation and some quick simulations. Warning: EXPERIMENTAL.
* Documentation progress.

# calmr 0.2.0

* Added multiple models to package and app (RW1972, PKH1982, MAC1975).
* Implementation of basic S4 classes for model, experiment, fit, and RSA comparison objects, as well as their methods.
* Added genetic algorithms (via `GA`) for parameter estimation.
* Added basic tools to perform representational similarity analysis.
* Documentation progress.

# calmr 0.1.0

* heidi is now calmr: Canonical Associative Learning Models in R. The package now aims to maintain several associative learning models and implement tools for  their use.
* Major overhaul of the training function (train_pav_model). All relevant calculations are now done as a function of all functional stimuli instead of just the US.
* Support for the specification of expectation/correction steps within the trial via ">". For example, the trial "A>(US)" will use only A to generate the expectation, but will learn about both stimuli during the correction step.
* The previous plotting function for R-values has been revamped to allow both simple and complex versions. The complex version facets r-values on a predictor basis, and uses colour lines for each target.
* Bugfix related to stimulus saliencies.

