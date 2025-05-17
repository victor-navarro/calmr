test_model <- "MAC1975"
simple_acquisition <- data.frame(g = "X", s1 = "10A>B")

pars <- get_parameters(simple_acquisition, model = test_model)

test_that("alphas work", {
  pars <- get_parameters(simple_acquisition, model = test_model)
  pars$alphas[1] <- 0.1
  res_low <- results(run_experiment(simple_acquisition,
    parameters = pars,
    outputs = "associations", model = test_model, seed = 0
  ))$associations
  res_low <- res_low[res_low$s1 == "A" & res_low$s2 == "B", "value"]
  res_low <- res_low[nrow(res_low)]

  pars$alphas[1] <- 0.8
  res_high <- results(run_experiment(simple_acquisition,
    parameters = pars,
    outputs = "associations", model = test_model, seed = 0
  ))$associations
  res_high <- res_high[res_high$s1 == "A" & res_high$s2 == "B", "value"]
  res_high <- res_high[nrow(res_high)]
  expect_true(res_high > res_low)
})

test_that("alphas increase for predictive stimuli", {
  simple_acquisition <- data.frame(
    g = c("seq", "none", "sim"),
    s1 = c("10A>B", "10A", "10AB")
  )
  pars <- get_parameters(simple_acquisition,
    model = test_model
  )
  exp <- run_experiment(simple_acquisition,
    parameters = pars,
    model = test_model, seed = 0
  )
  assocs <- results(exp)$associabilities
  assocs <- stats::aggregate(value ~ s1 + group, tail, 1, data = assocs)
  # df has seq, none, sim associabilities
  expect_equal(assocs$value[1], assocs$value[2])
  expect_equal(assocs$value[1], assocs$value[5])
  expect_equal(assocs$value[2], assocs$value[6])
  expect_true(assocs$value[3] > assocs$value[4])
})

fp_design <- data.frame(
  g = c("fp"),
  p1 = c("10AB>C/10A")
)
test_that("alphas change as a function of global/local error", {
  pars <- get_parameters(fp_design, model = test_model)
  exp <- run_experiment(fp_design,
    parameters = pars, model = test_model, seed = 0
  )
  assocs <- results(exp)$associabilities
  assocs <- stats::aggregate(value ~ s1 + group, tail, 1, data = assocs)
  # B is a better predictor of C
  # the associability of B increases, so it should be higher than that of C
  # the associability of A decreases, so it should be lower than that of C
  expect_true(assocs$value[1] < assocs$value[3])
  expect_true(assocs$value[2] > assocs$value[3])
})

test_that("min_alpha works", {
  pars <- get_parameters(fp_design, model = test_model)
  exp <- run_experiment(fp_design,
    parameters = pars, model = test_model, seed = 0
  )
  assocs <- results(exp)$associabilities
  assocs <- stats::aggregate(value ~ s1 + group, tail, 1, data = assocs)
  default_val <- assocs$value[1]
  pars$min_alphas["A"] <- 0.0001
  exp <- run_experiment(fp_design,
    parameters = pars, model = test_model, seed = 0
  )
  assocs <- results(exp)$associabilities
  assocs <- stats::aggregate(value ~ s1 + group, tail, 1, data = assocs)
  small_min_alpha <- assocs$value[1]
  expect_true(default_val > small_min_alpha)
})

test_that("betas_on work", {
  pars <- get_parameters(simple_acquisition, model = test_model)
  pars$betas_on[2] <- 0.1
  res_low <- results(run_experiment(simple_acquisition,
    parameters = pars, outputs = "associations",
    model = test_model, seed = 0
  ))$associations
  res_low <- res_low[res_low$s1 == "A" & res_low$s2 == "B", "value"]
  res_low <- res_low[nrow(res_low)]

  pars$betas_on[2] <- 0.8
  res_high <- results(run_experiment(simple_acquisition,
    parameters = pars, outputs = "associations",
    model = test_model, seed = 0
  ))$associations
  res_high <- res_high[res_high$s1 == "A" & res_high$s2 == "B", "value"]
  res_high <- res_high[nrow(res_high)]
  expect_true(res_high > res_low)
})

test_that("betas_off work", {
  des <- data.frame(g = "X", s1 = "10A>B", s2 = "10A")

  pars <- get_parameters(des, model = test_model)
  pars$betas_off[2] <- 0.1
  res_low <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = test_model, seed = 0
  ))$associations
  res_low <- res_low[res_low$s1 == "A" & res_low$s2 == "B", "value"]
  res_low <- res_low[nrow(res_low)]

  pars$betas_off[2] <- 0.8
  res_high <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = test_model, seed = 0
  ))$associations
  res_high <- res_high[res_high$s1 == "A" & res_high$s2 == "B", "value"]
  res_high <- res_high[nrow(res_high)]
  # res_high should extinguish faster
  expect_true(res_high < res_low)
})

test_that("lambdas work", {
  des <- data.frame(g = "X", s1 = "30A>B")
  pars <- get_parameters(des, model = test_model)
  pars$lambdas[2] <- 0.1
  res_low <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = test_model, seed = 0
  ))$associations
  res_low <- res_low[res_low$s1 == "A" & res_low$s2 == "B", "value"]
  res_low <- res_low[nrow(res_low)]

  pars$lambdas[2] <- 0.8
  res_high <- results(run_experiment(des,
    parameters = pars, outputs = "associations",
    model = test_model, seed = 0
  ))$associations
  res_high <- res_high[res_high$s1 == "A" & res_high$s2 == "B", "value"]
  res_high <- res_high[nrow(res_high)]

  # res_high should have a greater asymptote
  expect_true(res_high > res_low)
})

test_that("gammas work", {
  # the logic here is for the off group to be equal to the simple group
  # even though B is present in the design for off
  des <- data.frame(
    g = c("on", "off", "simple"),
    p1 = c("10A>BC", "10A>BC", "10A>C")
  )
  pars <- get_parameters(des, model = test_model)
  exp <- make_experiment(des, model = test_model, parameters = pars)
  exp@parameters$off$gammas["B"] <- 0
  exp <- run_experiment(exp, outputs = "associabilities")

  assocs <- results(exp)$associabilities
  assocs <- stats::aggregate(value ~ s1 + group, tail, 1, data = assocs)
  # table is a,b,c | off, on, simple

  # checks for associability of A
  # main check (off alpha A == simple alpha A)
  expect_equal(assocs$value[1], assocs$value[7])
  # A and B should be equal in the off group
  # because their associabilities only depend on local pred error on C
  expect_equal(assocs$value[1], assocs$value[2])
  # A in on should be greater in than A in off
  # because associability in the first is affected by pred error on B and C
  expect_true(assocs$value[4] > assocs$value[1])
  # neat extra one
  # C in simple should be greater than C in off
  # This is pred error of C counts for associability in the first case
  # (and C is artificially a bad predictor of itself)
  # but not in the second case
  expect_true(assocs$value[9] > assocs$value[3])
})
