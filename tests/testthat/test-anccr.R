rawdf <- data.frame(
  group = "G",
  p1 = c("10A>(US)/10B>(US)/10(US)"),
  r1 = TRUE
)

augment_args <- list(reward_labels = "US")
df <- parse_design(rawdf, model = "ANCCR", augment_args)

# can get parameters
params <- get_parameters(df, model = "ANCCR")
params <- set_reward_parameters(params, c("US"))

# can get arguments
ex <- make_experiment(df,
  parameters = params, model = "ANCCR"
)

source("R/ANCCR.R")

# can return raw values
run_experiment(ex, parse = FALSE, aggregate = FALSE)
