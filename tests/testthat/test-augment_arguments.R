rawdf <- data.frame(
  group = "G",
  p1 = "10A>(US)",
  r1 = FALSE,
  p2 = "10AB>(US)",
  r2 = FALSE
)
df <- parse_design(rawdf)
pars <- get_parameters(rawdf, model = "ANCCR")

test_that("augmenting ANCCR design creates more experience rows than trials", {
  exp <- experience(make_experiment(df,
    parameters = pars, model = "ANCCR"
  ))[[1]]
  expect_equal(nrow(exp), (20 + 30))
})

test_that("augmenting ANCCR design with jitter creates different timestamps", {
  nojitt <- pars
  nojitt$t_jitter <- 0
  exp_jit <- experience(make_experiment(df,
    parameters = pars, model = "ANCCR"
  ))[[1]]
  exp_nojit <- experience(make_experiment(df,
    parameters = nojitt, model = "ANCCR"
  ))[[1]]
  expect_true(length(unique(exp_jit$time)) != length(unique(exp_nojit$time)))
})
