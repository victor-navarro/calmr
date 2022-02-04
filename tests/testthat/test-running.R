test_that("model can run", {
  df = data.frame(Group = c("True", "Pseudo"),
                  P1 = c("10AB(US)/10AC", "5AB(US)/5AB/5AC(US)/5AC"),
                  R1 = c(TRUE, TRUE),
                  P2 = c("1A", "1A"),
                  R2 = c(TRUE, TRUE))
  pars = data.frame(Stimulus = c("A", "B", "C", "US"), Alpha = c(0.1, 0.2, 0.2, 0.3))
  #warning for not passing parameters
  expect_warning(run_heidi(df))
  #error for not passing enough parameters
  expect_error(run_heidi(df, param_df = pars[1, ]))
  #error for passing bad options
  expect_error(run_heidi(df, param_df = pars, options = list(mybadOption = T)))
  #model can run
  expect_named(run_heidi(df, param_df = pars), c("ws", "vs", "rs"))
  #multiple plots can be obtained
  expect_equal(length(make_plots(run_heidi(df, pars))), 6)
  #### trial_parser tests ####
  expect_setequal(
    trial_parser("10AB(US)")$stimuli, c("A", "B", "US"))
  expect_setequal(
    trial_parser("10AB(AB)(US)")$stimuli, c("A", "B", "AB", "US"))

})
