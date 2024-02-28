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
args <- make_experiment(df,
  parameters = pars, model = "ANCCR"
)

# can aggregate
x <- run_experiment(args)
res <- results(x)
plot(res$m_ij$value, res$psrcs$value[res$psrcs$type == "prc"])

# can plot
patch_plots(plot(x, type = "anccrs")) # adjusted net contingency
plot(x, type = "ncs") # adjusted net contingency



plot(x, type = "e_ij") # elegibility trace
plot(x, type = "e_i") # elegibility trace (???)
plot(x, type = "m_i") # base rate
plot(x, type = "m_ij") # more base rates (???)
plot(x, type = "nc") # net contingency

plot(x, type = "delta") # time delta (???)
plot(x, type = "psrcs") # fine
plot(x, type = "das") # dopamine
plot(x, type = "cws") # reward values
plot(x, type = "qs") # action values
