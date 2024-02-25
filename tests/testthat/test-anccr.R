rawdf <- data.frame(
  group = "G",
  p1 = c("20A>(US)"),
  r1 = TRUE
)

df <- parse_design(rawdf, model = "ANCCR")

# can get parameters
params <- get_parameters(df, model = "ANCCR")
params <- set_reward_parameters(params, c("US"))

# can get arguments
ex <- make_experiment(df,
  parameters = params, model = "ANCCR"
)

# can aggregate
# source("R/ANCCR.R")
x <- run_experiment(ex)

# can plot
plot(x)
supported_plots("ANCCR")
plot(x, type = "e_ij") # elegibility trace
plot(x, type = "e_i") # elegibility trace (???)
plot(x, type = "m_i") # base rate
plot(x, type = "m_ij") # more base rates (???)
plot(x, type = "nc") # net contingency
plot(x, type = "anccr") # adjusted net contingency
plot(x, type = "delta") # time delta (???)
plot(x, type = "psrcs") # fine
plot(x, type = "das") # dopamine
plot(x, type = "rews") # reward values
