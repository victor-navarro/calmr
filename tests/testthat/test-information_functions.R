# model_parameters
mods <- supported_models()
df <- parse_design(data.frame(group = "X", p1 = c("!10A/10B")))

test_that("model_parameters returns full information", {
  expect_true(length(model_parameters()) > 2)
})
test_that("model_parameters contains supported_models", {
  expect_true(all(length(sapply(mods, model_parameters)) > 0))
})
test_that("get_parameters works with model_parameters", {
  expect_true(all(
    length(sapply(mods, function(m) get_parameters(df, model = m))) > 0
  ))
})

test_that("supported_plots returns full information", {
  expect_true(length(supported_plots()) > 6)
})

test_that("model_outputs returns full information", {
  expect_true(length(model_outputs()) > 6)
})
