test_that("package works", {
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
  expect_named(run_heidi(df, param_df = pars), c("ws", "vs", "rs", "as"))
  #multiple plots can be obtained
  expect_equal(length(make_plots(run_heidi(df, pars))), 10)
  #### trial_parser tests ####
  expect_setequal(
    trial_parser("10AB(US)")$stimuli, c("A", "B", "US"))
  expect_setequal(
    trial_parser("10AB(AB)(US)")$stimuli, c("A", "B", "AB", "US"))
  expect_setequal(
    trial_parser("10A(US)")$stimuli, c("A", "US"))
  expect_setequal(
    trial_parser("10A(US_a)/10A(US_b)")$stimuli, c("A", "US_a", "US_b"))

  #thank you Dom
  #test for a case in which there is only one type of trial per cell
  simple_df = data.frame(Group = c("Over", "Ctrl"),
                         P1 = c("10AB(US)", "10A(US)"),
                         R1 = c(TRUE, TRUE),
                         P2 = c("1A", "1A"),
                         R2 = c(TRUE, TRUE))
  simple_pars = data.frame(Stimulus = c("A", "B", "US"), Alpha = c(0.1, 0.2, 0.3))
  expect_named(run_heidi(simple_df, param_df = simple_pars), c("ws", "vs", "rs", "as"))



})
