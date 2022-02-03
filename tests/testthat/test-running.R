test_that("model can run", {
  df = data.frame(Group = c("G1", "G2"), P1 = c("2A(US)/3B(US)", "10A(US)"), R1 = c(TRUE, FALSE))
  pars = data.frame(Stimulus = c("A", "B", "US"), Alpha = c(0.1, 0.2, 0.3))
  #warning for not passing parameters
  expect_warning(runHeidi(df))
  #error for not passing enough parameters
  expect_error(runHeidi(df, param_df = pars[1, ]))
  #error for passing bad options
  expect_error(runHeidi(df, param_df = pars, options = list(mybadOption = T)))
  #model can run
  expect_named(runHeidi(df, param_df = pars), c("ws", "vs", "rs"))
  #multiple plots can be obtained
  expect_equal(length(makePlots(runHeidi(df, pars))), 6)
  #trialParser tests
  expect_setequal(
    trialParser("10AB(US)")$stimuli, c("A", "B", "US"))
  expect_setequal(
    trialParser("10AB(AB)(US)")$stimuli, c("A", "B", "AB", "US"))
})
