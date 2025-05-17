# A problematic design
df <- data.frame(
  group = c("Blocking", "Control"),
  p1 = c("2N>(US)", ""),
  p2 = c("2NL>(US)", "2NL>(US)/2#L")
)

pars <- get_parameters(df, model = "TD")
tims <- get_timings(df, model = "TD")

test_that("can run without exponential", {
  noexp <- tims
  noexp$use_exponential <- 0
  expect_no_error(
    run_experiment(df,
      parameters = pars,
      timings = noexp,
      model = "TD"
    )
  )
})

# A specific test with nested trials
df <- data.frame(
  group = "G",
  p1 = "!10A>AB>(US)"
)

test_that("can run with nested trials", {
  tims <- get_timings(df, "TD")
  tims$period_ts$stimulus_duration[c(1:3)][] <- 6
  tims$transition_ts$transition_delay[] <- 0
  pars <- get_parameters(df, model = "TD")
  pars$alphas[] <- .3

  expect_no_error(run_experiment(df,
    model = "TD",
    timings = tims,
    parameters = pars,
    parse = TRUE,
    aggregate = TRUE
  ))
})
