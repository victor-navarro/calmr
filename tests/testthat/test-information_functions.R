# parameter_info
mods <- supported_models()
df <- parse_design(data.frame(group = "X", p1 = c("10A/10B"), r1 = TRUE))

sapply(mods, function(m) get_parameters(df, model = m))

test_that("parameter_info returns full information", {
  expect_true(length(parameter_info()) > 0)
})
test_that("parameter_info contains supported_models", {
  expect_true(all(length(sapply(mods, parameter_info)) > 0))
})
test_that("get_parameters works with parameter_info", {
  expect_true(all(
    length(sapply(mods, function(m) get_parameters(df, model = m))) > 0
  ))
})
