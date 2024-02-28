rm(list = ls(all = TRUE))
df <- data.frame(
  group = c("FP", "FN"),
  phase1 = c("100F>T>(US)/100T", "100F>T/100T>(US)"),
  r1 = c(TRUE, TRUE)
)
pars <- get_parameters(df, model = "ANCCR")
# pars <- set_reward_parameters(pars, rewards = "US")
pars$alpha_reward <- .8
pars$alpha <- 0.08
pars$sampling_interval <- 5 # reduce sampling rate by a ton
pars$temperature <- 10

args <- make_experiment(df,
  parameters = pars, model = "ANCCR"
)


# can aggregate
x <- run_experiment(args)
res <- results(x)

# can plot
all_plots <- plot(x)

plot(x, type = "ps")
