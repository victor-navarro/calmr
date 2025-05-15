des <- data.frame(g = "X", s1 = "10A>B")

test_that("alphas work", {
  pars <- get_parameters(des, model = "RW1972")
  pars$alphas[1] <- 0.1
  res_low <- results(run_experiment(des,
    parameters = pars,
    outputs = "associations", model = "RW1972", seed = 0
  ))$associations
  res_low <- res_low[res_low$s1 == "A" & res_low$s2 == "B", "value"]
  res_low <- res_low[nrow(res_low)]

  pars$alphas[1] <- 0.8
  res_high <- results(run_experiment(des,
    parameters = pars,
    outputs = "associations", model = "RW1972", seed = 0
  ))$associations
  res_high <- res_high[res_high$s1 == "A" & res_high$s2 == "B", "value"]
  res_high <- res_high[nrow(res_high)]
  expect_true(res_high > res_low)
})

test_that("betas_on work", {
  pars <- get_parameters(des, model = "RW1972")
  pars$betas_on[2] <- 0.1
  res_low <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = "RW1972", seed = 0
  ))$associations
  res_low <- res_low[res_low$s1 == "A" & res_low$s2 == "B", "value"]
  res_low <- res_low[nrow(res_low)]

  pars$betas_on[2] <- 0.8
  res_high <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = "RW1972", seed = 0
  ))$associations
  res_high <- res_high[res_high$s1 == "A" & res_high$s2 == "B", "value"]
  res_high <- res_high[nrow(res_high)]
  expect_true(res_high > res_low)
})

test_that("betas_off work", {
  des <- data.frame(g = "X", s1 = "10A>B", s2 = "10A")

  pars <- get_parameters(des, model = "RW1972")
  pars$betas_off[2] <- 0.1
  res_low <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = "RW1972", seed = 0
  ))$associations
  res_low <- res_low[res_low$s1 == "A" & res_low$s2 == "B", "value"]
  res_low <- res_low[nrow(res_low)]

  pars$betas_off[2] <- 0.8
  res_high <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = "RW1972", seed = 0
  ))$associations
  res_high <- res_high[res_high$s1 == "A" & res_high$s2 == "B", "value"]
  res_high <- res_high[nrow(res_high)]
  # res_high should extinguish faster
  expect_true(res_high < res_low)
})

test_that("lambdas work", {
  des <- data.frame(g = "X", s1 = "30A>B")
  pars <- get_parameters(des, model = "RW1972")
  pars$lambdas[2] <- 0.1
  res_low <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = "RW1972", seed = 0
  ))$associations
  res_low <- res_low[res_low$s1 == "A" & res_low$s2 == "B", "value"]
  res_low <- res_low[nrow(res_low)]

  pars$lambdas[2] <- 0.8
  res_high <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = "RW1972", seed = 0
  ))$associations
  res_high <- res_high[res_high$s1 == "A" & res_high$s2 == "B", "value"]
  res_high <- res_high[nrow(res_high)]

  # res_high should have a greater asymptote
  expect_true(res_high > res_low)
})
