df <- get_design("blocking")
pars <- get_parameters(df, model = "RW1972")
exper <- make_experiment(df, parameters = pars, model = "RW1972")

test_that("can run without parsing/aggregating", {
  res <- run_experiment(exper, parse = FALSE, aggregate = FALSE)
  expect_true(length(raw_results(res)) > 1)
})
test_that("can run with parsing", {
  res <- run_experiment(exper, parse = TRUE, aggregate = FALSE)
  expect_true(length(parsed_results(res)) > 1)
})
test_that("can run with parsing/aggregating", {
  res <- run_experiment(exper)
  expect_true(length(results(res)) > 1)
})

test_that("run_experiment asserts correctly", {
  # warning for not passing parameters
  expect_warning(run_experiment(
    df,
    model = "HD2022"
  ))
  # TODO: write tests that check for parameters and for options
})

test_that("run_experiment runs with split args", {
  res <- run_experiment(df,
    parameters = get_parameters(df, model = "HD2022"),
    model = "HD2022"
  )
  expect_named(
    results(res)
  )
})

test_that("run_experiment runs with bundled args", {
  args <- make_experiment(df,
    parameters = get_parameters(df, model = "HD2022"),
    model = "HD2022"
  )
  res <- run_experiment(args)
  expect_named(
    results(res)
  )
})

test_that("run_experiment works with simple cells", {
  # test for a case in which there is only one
  # type of trial per cell (thanks Dom)
  simple_df <- data.frame(
    Group = c("Over", "Ctrl"),
    P1 = c("1AB(US)", "1A(US)"),
    P2 = c("1A", "1A")
  )
  simple_pars <- get_parameters(simple_df, model = "HD2022")
  res <- run_experiment(
    simple_df,
    parameters = simple_pars, model = "HD2022"
  )
  expect_named(
    results(res)
  )
})

test_that("run_experiment stops overly minimal experiments", {
  df <- data.frame(group = "A", p1 = "!1A")
  expect_error(run_experiment(
    df,
    model = "HD2022",
    parameters = get_parameters(df, model = "HD2022")
  ))
})
