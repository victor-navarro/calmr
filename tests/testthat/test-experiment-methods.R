df <- get_design("relative_validity")

exper <- make_experiment(df,
  model = "RW1972",
  parameters = get_parameters(df, model = "RW1972")
)

test_that("parameters retrieves the parameters", {
  expect_named(parameters(exper))
})

test_that("parameters<- sets the parameters", {
  oldpars <- parameters(exper)[[1]]
  pars <- get_parameters(df, model = "RW1972")
  pars$betas_on["US"] <- 0.7
  parameters(exper) <- pars
  newpars <- parameters(exper)[[1]]
  expect_true(newpars$betas_on["US"] != oldpars$betas_on["US"])
})

test_that("parameters<- throws error with weird list", {
  expect_error(parameters(exper) <- list("asdf" = 1))
})

test_that("parameters<- throws error with partial list", {
  pars <- parameters(exper)
  pars[[1]] <- pars[[1]][-1]
  expect_error(parameters(exper) <- pars)
})
