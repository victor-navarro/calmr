# A problematic design
df <- data.frame(
  group = c("Blocking", "Control"),
  p1 = c("2N>(US)", ""), r1 = FALSE,
  p2 = c("2NL>(US)", "2NL>(US)/2#L"), r2 = FALSE
)

pars <- get_parameters(df, model = "TD")
tims <- get_timings(df)

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
