# .comparator_proc
df <- get_design("blocking")
pars <- get_parameters(df, model = "SM2007")

test_that("SM2007 works at the zero order", {
  high_pars <- pars
  high_pars$order <- 0
  expect_no_error(
    run_experiment(df,
      parameters = high_pars,
      model = "SM2007"
    )
  )
})

test_that("SM2007 works at higher orders", {
  high_pars <- pars
  high_pars$order <- 2
  expect_no_error(
    run_experiment(df,
      parameters = high_pars,
      model = "SM2007"
    )
  )
  # and a call to capture the output
  # capture_message does not work with too many messages :(
  expect_no_error(capture_message({
    res <- run_experiment(df,
      parameters = high_pars,
      model = "SM2007",
      debug = TRUE
    )
  }))
})

test_that("SM2007 works with the general comparator function", {
  high_pars <- pars
  high_pars$order <- 3
  expect_no_error(
    run_experiment(df,
      parameters = high_pars,
      model = "SM2007",
      comparator_func = .comparator_proc
    )
  )
  # and a call to capture the output
  expect_no_error(capture_message({
    res <- run_experiment(df,
      parameters = pars,
      model = "SM2007",
      comparator_func = .comparator_proc,
      debug = TRUE
    )
  }))
})
