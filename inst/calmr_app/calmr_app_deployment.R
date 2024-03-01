# restart your RStudio/R
# please remember to install the latest version of calmr from your github page
devtools::install_github("victor-navarro/calmr", force = TRUE)
rsconnect::deployApp("C:/Users/sapvn2/calmr/inst/calmr_app",
  forceUpdate = TRUE
)
