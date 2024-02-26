df <- data.frame(
  group = "G",
  p1 = c("100N>(US)"),
  r1 = TRUE
)
pars <- get_parameters(df, model = "ANCCR")
# pars <- set_reward_parameters(pars, rewards = "US")
# pars$alpha_reward <- .8
# pars$alpha <- 0.08
# pars$sampling_interval <- 5 # reduce sampling rate by a ton
args <- make_experiment(df,
  parameters = pars, model = "ANCCR"
)

# can aggregate
x <- run_experiment(args)

# can plot
supported_plots("ANCCR")

plot(x, type = "anccrs") # adjusted net contingency
plot(x, type = "ncs") # adjusted net contingency



plot(x, type = "e_ij") # elegibility trace
plot(x, type = "e_i") # elegibility trace (???)
plot(x, type = "m_i") # base rate
plot(x, type = "m_ij") # more base rates (???)
plot(x, type = "nc") # net contingency

plot(x, type = "delta") # time delta (???)
plot(x, type = "psrcs") # fine
plot(x, type = "das") # dopamine
plot(x, type = "rews") # reward values
plot(x, type = "qs") # action values
