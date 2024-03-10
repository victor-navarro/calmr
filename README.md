# calm

Canonical Associative Learning Models

## Installation

You will need devtools to install this package from github. If you do not have it, run:

`install.packages("devtools")`

Afterward, you can install directly from this repository via:

`devtools::install_github("victor-navarro/calm", build_vignettes = TRUE)`

If you managed to build the vignettes, there's a vignette showing the basics of the package in

`vignette("calm_basics", package = "calm")`

If you want to do some simulations using the companion app, you must install the `calm.app` package and then launch the app.

```
devtools::install_github("victor-navarro/calm.app")
calm.app::launch_app()
```

## Try the online Shiny app

If you want to check the app without committing to an install, you can check it out here (be wary: the server might have run out of the free monthly quota).

[https://victor-navarro.shinyapps.io/calm_app/](https://victor-navarro.shinyapps.io/calm_app/)

