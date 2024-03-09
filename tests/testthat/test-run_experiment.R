df <- get_design("relative_validity")
models <- c("RW1972", "MAC1975")
df <- parse_design(df)

experiments <- lapply(
  models,
  function(m) {
    make_experiment(df,
      model = m,
      parameters = get_parameters(df, model = m),
      options = get_exp_opts(iterations = 2)
    )
  }
)

test_that("can run without parsing/aggregating", {
  res <- run_experiment(experiments[[1]], parse = FALSE, aggregate = FALSE)
  expect_true(length(raw_results(res)) > 1)
})
test_that("can run with parsing", {
  res <- run_experiment(experiments[[1]], parse = TRUE, aggregate = FALSE)
  expect_true(length(parsed_results(res)) > 1)
})

test_that("can run with parsing/aggregating", {
  res <- run_experiment(experiments[[1]])
  expect_true(length(results(res)) > 1)
})

test_that("run_experiment asserts correctly", {
  # warning for not passing parameters
  expect_warning(run_experiment(
    df,
    model = "HD2022", options = get_exp_opts()
  ))
  # error for not passing enough parameters
  expect_error(run_experiment(df,
    parameters = pars[1, ],
    model = "HD2022", options = get_exp_opts()
  ))
  # error for passing bad options
  expect_error(run_experiment(df,
    parameters = pars,
    model = "HD2022", options = list(mybadOption = TRUE)
  ))
})

test_that("run_experiment runs with split args", {
  res <- run_experiment(df,
    parameters = pars,
    model = "HD2022", options = get_exp_opts()
  )
  expect_named(
    results(res)
  )
})

test_that("run_experiment runs with bundled args", {
  args <- make_experiment(df,
    parameters = pars,
    model = "HD2022", options = get_exp_opts()
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
    R1 = c(TRUE, TRUE),
    P2 = c("1A", "1A"),
    R2 = c(TRUE, TRUE)
  )
  simple_pars <- get_parameters(simple_df, model = "HD2022")
  res <- run_experiment(
    simple_df,
    parameters = simple_pars, model = "HD2022",
    options = get_exp_opts()
  )
  expect_named(
    results(res)
  )
})

test_that("run_experiment stops overly minimal experiments", {
  df <- data.frame(group = "A", p1 = "1A", r1 = TRUE)
  expect_error(run_experiment(
    df,
    model = "HD2022",
    parameters = get_parameters(df, model = "HD2022"),
    options = get_exp_opts()
  ))
})
