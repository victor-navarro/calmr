# heidi 0.1.0

* Major overhaul of the training function (train_pav_heidi). All relevant calculations are now done as a function of all functional stimuli instead of just the US.
* Support for the specification of expectation/correction steps within the trial via ">". For example, the trial "A>(US)" will use only A to generate the expectation, but will learn about both stimuli during the correction step.
* The previous plotting function for R-values has been revamped to allow both simple and complex versions. The complex version facets r-values on a predictor basis, and uses colour lines for each target.
* Bugfix related to stimulus saliencies.

# heidi 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
