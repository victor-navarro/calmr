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

test_that("show method works", {
  expect_no_error(capture_message(show(exper)))
})

test_that("design method works", {
  expect_no_error(design(exper))
})

test_that("trials method works", {
  expect_no_error(trials(exper))
})

ran_exper <- run_experiment(exper)
raw_exper <- run_experiment(exper, parse = FALSE, aggregate = FALSE)
test_that("results returns aggregated results", {
  expect_named(results(ran_exper))
})

test_that("results returns aggregated results", {
  expect_named(parsed_results(ran_exper)[[1]])
})

test_that("raw_results returns raw results", {
  expect_true(all(sapply(raw_results(ran_exper), lapply, class) == "array"))
})

test_that("parse method works", {
  expect_no_error(parse(raw_exper))
})

test_that("aggregate method works", {
  expect_no_error(aggregate(raw_exper))
})

test_that("parse method throws error without raw_results", {
  expect_error(parse(exper))
})

test_that("plot method throws error when there are no agregated_results", {
  expect_error(plot(parse(raw_exper)))
})

test_that("graph method throws error when there are no agregated_results", {
  expect_error(graph(parse(raw_exper)))
})
