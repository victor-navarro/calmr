rm(list = ls(all = TRUE))
library(calmr)
future::plan(future::sequential, split = TRUE)
# future::plan(future::multisession)
# calmr_verbosity(TRUE)
calmr_verbosity(FALSE)

# LONG EXP
df <- data.frame(
  group = c("Ctrl", "Exp"),
  phase1 = c("200C>(US)", "200A>(US)"),
  r1 = FALSE,
  phase2 = c("200AB>(US)", "200AB>(US)"),
  r2 = FALSE
)

pars <- get_parameters(df, model = "ANCCR")
args <- make_experiment(df,
  parameters = pars, model = "ANCCR",
  options = get_exp_opts(iterations = 1)
)

x <- run_experiment(args)

plot(x, type = "cws")
