df <- data.frame(g = "g", p1 = "2A/2AB", r1 = TRUE)
pars <- get_parameters(df, model = "ANCCR")

test_that("can build experiment without transitions", {
  exp <- make_experiment(df, parameters = pars, model = "ANCCR")
  expect_true(nrow(experiences(exp)[[1]]) > 1)
})
