# will throw  errors on stack
test_that("calmr_verbosity can be turned on", {
  expect_error(calmr_verbosity(TRUE))
})

test_that("calmr_verbosity can be turned off", {
  expect_error(calmr_verbosity(FALSE))
})
