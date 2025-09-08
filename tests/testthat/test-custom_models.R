# testing that custom models can be loaded and run
source("support_files/OJA_demo_class.R")

df <- data.frame(
  Group = c("True"),
  P1 = c("!2AB(US)/2AC")
)
pars <- get_parameters(df, model = "OJA")
exp <- make_experiment(df,
  model = "OJA",
  parameters = pars,
  iterations = 1
)

test_that("custom model can run", {
  res <- run_experiment(exp)
  expect_named(results(res))
})
