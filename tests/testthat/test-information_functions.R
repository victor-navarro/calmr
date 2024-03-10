# parameter_info
mods <- supported_models()
df <- parse_design(data.frame(group = "X", p1 = c("10A/10B"), r1 = TRUE))

test_that("parameter_info returns full information", {
  expect_true(length(parameter_info()) > 2)
})
test_that("parameter_info contains supported_models", {
  expect_true(all(length(sapply(mods, parameter_info)) > 0))
})
test_that("get_parameters works with parameter_info", {
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
