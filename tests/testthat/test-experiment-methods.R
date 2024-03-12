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

raw_exper <- run_experiment(exper, parse = FALSE, aggregate = FALSE)
parsed_exper <- parse(raw_exper)
agg_exper <- aggregate(parsed_exper)
test_that("results returns aggregated results", {
  expect_named(results(agg_exper))
})

test_that("results returns aggregated results", {
  expect_named(parsed_results(agg_exper)[[1]])
})

test_that("raw_results returns raw results", {
  expect_true(all(sapply(raw_results(agg_exper), lapply, class) == "array"))
})

test_that("aggregate method works does not work with raw experiments", {
  expect_error(aggregate(raw_exper))
})

test_that("parse method throws error without raw_results", {
  expect_error(parse(exper))
})

test_that("plot method throws error when missing aggregated results", {
  expect_error(plot(raw_exper, type = "vs"))
})

test_that("plot method warns when missing aggregated output", {
  pagg <- aggregate(parse(raw_exper), outputs = "rs")
  expect_warning(plot(pagg, type = "vs"))
})

test_that("graph method throws error when there are no agregated_results", {
  expect_error(graph(parse(raw_exper)))
})

test_that("parse method will return only some outputs", {
  expect_setequal(
    names(
      parsed_results(parse(raw_exper, outputs = "vs"))[[1]]
    ),
    c("vs")
  )
})

test_that("parse method is able to parse partially parsed experiments", {
  pparsed <- parse(raw_exper, outputs = "vs")
  expect_setequal(
    names(parsed_results(parse(pparsed, outputs = "rs"))[[1]]),
    c("vs", "rs")
  )
  # can skip parsing
  expect_setequal(
    names(parsed_results(parse(pparsed, outputs = "vs"))[[1]]),
    c("vs")
  )
})

test_that("parse method throws errors with bad outputs", {
  expect_error(
    parse(raw_exper, outputs = "os")
  )
})

test_that("aggregate method throws errors with bad outputs", {
  expect_error(
    aggregate(agg_exper, outputs = "os")
  )
})

test_that("aggregate method is able to agg partially parsed experiments", {
  pparsed <- parse(raw_exper, outputs = "vs")
  # can aggregate existing parsed_results
  expect_setequal("vs", names(results(aggregate(pparsed, outputs = "vs"))))
  # should aggregate nonexisting parsed results (would involve parallel woes)
  expect_error(aggregate(pparsed, outputs = "rs"))
  # uses output sanitization
  expect_warning(aggregate(pparsed, outputs = c("vs", "os")))
  # but can work in tandem from the beginning
  expect_setequal(
    "vs",
    names(results(run_experiment(exper, outputs = c("vs"))))
  )
})
