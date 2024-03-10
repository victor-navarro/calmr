# .comparator_proc
df <- get_design("blocking")
pars <- get_parameters(df, model = "SM2007")
pars$order <- 2

test_that("SM2007 works at higher orders", {
  expect_no_error(run_experiment(df,
    parameters = pars, model = "SM2007",
    comparator_func = .comparator_proc
  ))
})

test_that("softmax function .soft works", {
  expect_equal(.5, .soft(c(1, 1))[1])
})
