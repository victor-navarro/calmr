df <- data.frame(
  Group = c("True", "Pseudo"),
  P1 = c("10AB(US)/10AC", "5AB(US)/5AB/5AC(US)/5AC"),
  R1 = c(TRUE, TRUE),
  P2 = c("1A", "1A"),
  R2 = c(TRUE, TRUE)
)
pars <- data.frame(
  stimulus = c("A", "B", "C", "US"),
  alphas = c(0.1, 0.2, 0.2, 0.3)
)

run_experiment(
  df,
  model = "HD2022", options = get_exp_opts()
)
asdf
test_that("run_experiment asserts correctly", {
  # warning for not passing parameters
  expect_warning(run_experiment(
    df,
    model = "HD2022", options = get_exp_opts()
  ))
  # error for not passing enough parameters
  expect_error(run_experiment(df,
    param_df = pars[1, ],
    model = "HD2022", options = get_exp_opts()
  ))
  # error for passing bad options
  expect_error(run_experiment(df,
    param_df = pars,
    model = "HD2022", options = list(mybadOption = TRUE)
  ))
})

test_that("run_experiment works", {
  # test for a case in which there is only one type of trial per cell (thanks Dom)
  simple_df <- data.frame(
    Group = c("Over", "Ctrl"),
    P1 = c("10AB(US)", "10A(US)"),
    R1 = c(TRUE, TRUE),
    P2 = c("1A", "1A"),
    R2 = c(TRUE, TRUE)
  )
  simple_pars <- data.frame(stimulus = c("A", "B", "US"), alphas = c(0.1, 0.2, 0.3))
  expect_named(run_experiment(
    simple_df,
    param_df = simple_pars, model = "HD2022",
    options = get_exp_opts()
  )@parsed_results, c("vs", "rs", "as", "acts"))
})

test_that("run_experiment stops overly minimal experiments", {
  df <- data.frame(group = "A", p1 = "1A", r1 = T)
  model <- "HD2022"
  expect_error(run_experiment(
    df,
    model = model,
    param_df = get_parameters(df, model = model),
    options = get_exp_opts()
  ))
})
