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

* heidi is now calmr: Canonical Associative Learning Models in R. The package now aims to maintain several associative learning models and implement tools for the their use.
* Major overhaul of the training function (train_pav_model). All relevant calculations are now done as a function of all functional stimuli instead of just the US.
* Support for the specification of expectation/correction steps within the trial via ">". For example, the trial "A>(US)" will use only A to generate the expectation, but will learn about both stimuli during the correction step.
* The previous plotting function for R-values has been revamped to allow both simple and complex versions. The complex version facets r-values on a predictor basis, and uses colour lines for each target.
* Bugfix related to stimulus saliencies.

