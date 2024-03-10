# will throw  errors on stack
test_that("calm_verbosity can be turned on", {
  expect_error(calm_verbosity(TRUE))
})

test_that("calm_verbosity can be turned off", {
  expect_error(calm_verbosity(FALSE))
})
