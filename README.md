# calmr

Canonical Associative Learning Models and their Representations

<!-- badges: start -->
[![R-CMD-check](https://github.com/victor-navarro/calmr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/victor-navarro/calmr/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/victor-navarro/calmr/graph/badge.svg?token=8VTS3MQX51)](https://app.codecov.io/gh/victor-navarro/calmr)
![cran_downloads](https://cranlogs.r-pkg.org/badges/grand-total/calmr)
<!-- badges: end -->

## Installing the latest stable version

You may install the latest stable version from CRAN:

```
install.packages("calmr")
```

## Installing the latest version

If you are feeling more daring, you can install the latest version of the package. You will need devtools to install this package from GitHub.

```
install.packages("devtools")
devtools::install_github("victor-navarro/calmr")
```

If you managed to build the vignettes, there's a vignette showing the basics of the package. (Worry not, the package's [website](https://victornavarro.org/calmr/) also has it).

```
vignette("calmr_basics", package = "calmr")
```

If you want to do some simulations using the companion app, you must install the `calmr.app` package and then launch the app.

```
devtools::install_github("victor-navarro/calmr.app")
calmr.app::launch_app()
```

## Try the online Shiny app

If you want to check the app without committing to an install, you can check it out here (be wary: the server might have run out of the free monthly quota).

[https://victor-navarro.shinyapps.io/calmr_app/](https://victor-navarro.shinyapps.io/calmr_app/)

