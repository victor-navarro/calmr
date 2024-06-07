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

test_that("filter method filters with single filters", {
  onlya <- results(filter(agg_exper, stimuli = "A"))$associations
  expect_setequal(unique(onlya[, c("s1", "s2")]), c("A", "A"))
  ab <- results(filter(agg_exper, stimuli = c("A", "B")))$associations
  expect_setequal(unlist(unique(ab[, c("s1")])), c("A", "B"))
  abp1 <- results(filter(agg_exper,
    stimuli = c("A", "B"),
    phase = c("P1")
  ))$associations
  expect_true(all(c("A", "B", "P1") %in%
    c(unique(abp1$s1), unique(abp1$phase))))
  tn <- trials(agg_exper)$trial_names[1]
  abus <- results(filter(agg_exper, trial_types = c(tn)))$responses
  expect_true(all(abus$trial_type == tn))
})

test_that("plot method throws error when missing aggregated results", {
  expect_error(plot(raw_exper, type = "associations"))
})

test_that("plot method warns when missing aggregated output", {
  pagg <- aggregate(parse(raw_exper), outputs = "responses")
  expect_warning(plot(pagg, type = "associations"))
})

test_that("graph method throws error when there are no agregated_results", {
  expect_error(graph(parse(raw_exper)))
})

test_that("parse method will return only some outputs", {
  expect_setequal(
    names(
      parsed_results(parse(raw_exper, outputs = "associations"))[[1]]
    ),
    c("associations")
  )
})

test_that("parse method is able to parse partially parsed experiments", {
  pparsed <- parse(raw_exper, outputs = "associations")
  expect_setequal(
    names(parsed_results(parse(pparsed, outputs = "responses"))[[1]]),
    c("associations", "responses")
  )
  # can skip parsing
  expect_setequal(
    names(parsed_results(parse(pparsed, outputs = "associations"))[[1]]),
    c("associations")
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
  pparsed <- parse(raw_exper, outputs = "associations")
  # can aggregate existing parsed_results
  expect_setequal("associations", names(results(aggregate(pparsed, outputs = "associations"))))
  # should aggregate nonexisting parsed results (would involve parallel woes)
  expect_error(aggregate(pparsed, outputs = "responses"))
  # uses output sanitization
  expect_warning(aggregate(pparsed, outputs = c("associations", "os")))
  # but can work in tandem from the beginning
  expect_setequal(
    "associations",
    names(results(run_experiment(exper, outputs = c("associations"))))
  )
})

test_that("experiences retrieves the experiences", {
  expect_true(length(experiences(exper)) == 2)
})

test_that("experiences<- sets the experiences", {
  old_exper <- experiences(exper)
  old_exper[[1]][1, "tn"] <- "TEST"
  experiences(exper) <- old_exper
  newexp <- experiences(exper)
  expect_true(newexp[[1]][1, "tn"] == "TEST")
})

test_that("experiences<- throws error with weird list", {
  expect_error(experiences(exper) <- rep(list("asdf" = 1), 3))
})


tim_exper <- make_experiment(df,
  parameters = get_parameters(df, model = "TD"),
  timings = get_timings(df, model = "TD"), model = "TD"
)

test_that("timings retrieves the timings", {
  expect_named(timings(tim_exper))
})

test_that("timings<- sets the timings", {
  oldtims <- timings(tim_exper)
  oldtims$time_resolution <- 0.7
  timings(tim_exper) <- oldtims
  expect_true(timings(tim_exper)$time_resolution == oldtims$time_resolution)
})

test_that("timings<- throws error with weird list", {
  expect_error(timings(tim_exper) <- list("asdf" = 1))
})

test_that("timings<- throws error with partial list", {
  tims <- timings(tim_exper)
  tims[[1]] <- tims[[1]][-1]
  expect_error(timings(tim_exper) <- tims)
})
