#restart your RStudio/R
#please remember to install the latest version of calmr from your github page
devtools::install_github("victor-navarro/calmr", force = TRUE)
rsconnect::deployApp("~/OneDrive - Cardiff University/calmr_full/calmr/inst/calmr_app",
                     forceUpdate = TRUE)
rsconnect::deployApp("C:/Users/sapvn2/OneDrive - Cardiff University/calmr_full/calmr/inst/calmr_app",
                     forceUpdate = TRUE)

