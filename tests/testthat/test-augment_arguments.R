rawdf <- data.frame(
  group = "G",
  p1 = "10A>(US)",
  p2 = "10AB>(US)"
)
df <- parse_design(rawdf)
pars <- get_parameters(rawdf, model = "ANCCR")
tims <- get_timings(rawdf, model = "ANCCR")

test_that("augmenting ANCCR design creates more experience rows than trials", {
  exp <- experiences(make_experiment(df,
    parameters = pars, model = "ANCCR",
    timings = tims
  ))[[1]]
  expect_equal(nrow(exp), (20 + 30))
})

test_that("augmenting ANCCR design with jitter creates different timestamps", {
  nojitt <- pars
  nojitt$jitter <- 0
  exp_jit <- experiences(make_experiment(df,
    parameters = pars, model = "ANCCR",
    timings = tims
  ))[[1]]
  exp_nojit <- experiences(make_experiment(df,
    parameters = nojitt, model = "ANCCR",
    timings = tims
  ))[[1]]
  expect_true(length(unique(exp_jit$time)) != length(unique(exp_nojit$time)))
})


test_that("can get TD design with no sampling", {
  pars <- get_parameters(df, model = "TD")
  tims <- get_timings(df, model = "TD")
  tims$sample_timings <- FALSE
  expect_no_error(make_experiment(df,
    parameters = pars,
    timings = tims, model = "TD"
  ))
})
