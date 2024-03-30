# A problematic design
df <- data.frame(
  group = c("Blocking", "Control"),
  p1 = c("2N>(US)", ""), r1 = FALSE,
  p2 = c("2NL>(US)", "2NL>(US)/2#L"), r2 = FALSE
)

pars <- get_parameters(df, model = "ANCCR")
tims <- get_timings(df)

test_that("set_reward_parameters returns consistent values", {
  expect_equal(pars, set_reward_parameters(pars, names(pars$betas)))
})

test_that("set reward_parameters changes values", {
  expect_true(pars$betas["N"] != set_reward_parameters(pars, "US")$betas["N"])
})

test_that("can run without exponential", {
  noexp <- tims
  noexp$use_exponential <- 0
  expect_no_error(
    run_experiment(df,
      parameters = pars,
      timings = noexp,
      model = "ANCCR"
    )
  )
})

test_that("can run with timed alpha", {
  talpha <- pars
  talpha$use_timed_alpha <- 1
  expect_no_error(run_experiment(df,
    parameters = talpha,
    timings = tims, model = "ANCCR"
  ))
})

test_that("can run with exact mean", {
  exac <- pars
  exac$use_exact_mean <- 1
  expect_no_error(run_experiment(df,
    parameters = exac,
    timings = tims, model = "ANCCR"
  ))
})
