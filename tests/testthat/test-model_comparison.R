# By making experiment beforehand (recommended)
df <- data.frame(g = "A", p1 = "!2A>(US)")
models <- c("HD2022", "RW1972", "PKH1982")
exps <- lapply(models, function(m) {
  make_experiment(df,
    parameters = get_parameters(df, model = m),
    model = m
  )
})
test_that("compare_models works with experiment list", {
  expect_true(length(compare_models(exps)) == 3)
})

test_that("compare_models works with partial arguments", {
  expect_true(length(compare_models(df, models = models)) == 3)
})

test_that("compare models throws error if the design is already parsed", {
  expect_error(compare_models(parse_design(df), models = models))
})

test_that("compare models throws error if user passes design but no models", {
  expect_error(compare_models(df))
})
