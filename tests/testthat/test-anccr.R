exp <- data.frame(
  group = "G",
  p1 = c("10A>(US)/10B>(US)/10(US)"),
  r1 = TRUE
)

# can get parameters
params <- get_parameters(exp, model = "ANCCR")

run_time_experiment(exp)
