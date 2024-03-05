rm(list = ls(all = TRUE))
future::plan(future::sequential, split = TRUE)
# future::plan(future::multisession)
calmr_verbosity(TRUE)
# calmr_verbosity(FALSE)

# LONG EXP
df <- data.frame(
  group = c("Ctrl", "Exp"),
  phase1 = c("4000C>(US)", "4000A>(US)"),
  r1 = FALSE,
  phase2 = c("1500AB>(US)", "1500AB>(US)"),
  r2 = FALSE
)

pars <- get_parameters(df, model = "ANCCR")
args <- make_experiment(df,
  parameters = pars, model = "ANCCR",
  options = get_exp_opts(iterations = 1)
)

profvis::profvis({
})

x <- run_experiment(args, debug_t = -403, parse = FALSE)
