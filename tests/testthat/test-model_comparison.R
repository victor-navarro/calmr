exp <- data.frame(
  Group = c("A", "B"),
  P1 = c("2(A)>(US)/1B>(US)", "1(A)>(US)/2B>(US)"),
  R1 = TRUE
)
exp <- parse_design(exp)
models <- c("HD2022", "RW1972", "PKH1982")
options <- get_exp_opts()
parameters <- sapply(models, get_parameters, design = exp)

test_that("compare_models works with full parameters", {
  comp <- compare_models(exp,
    models = models,
    parameters = parameters, options = options
  )
  expect_named(results(comp))
})

test_that("compare_models works with some parameters", {
  comp <- suppressWarnings(
    compare_models(exp, models = models)
  )
  expect_named(results(comp))
})
