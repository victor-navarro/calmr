test_that(".calm_assert warns with NULL model", {
  expect_warning(.calm_assert("supported_model", NULL))
})

test_that(".calm_assert throws error with weird model", {
  expect_error(.calm_assert("supported_model", "Weird"))
})

test_that(".calm_assert throws error with no design", {
  expect_error(.calm_assert("parsed_design", NULL))
})

test_that(".calm_assert throws error with weird optimizer", {
  expect_error(.calm_assert("supported_optimizer", "Weird"))
})

test_that(".calm_assert throws error with weird family", {
  expect_error(.calm_assert("supported_family", "Weird"))
})

test_that(".calm_assert throws error for upper and lower limits with NAs", {
  expect_error(.calm_assert(
    "limits_OK",
    list(ll = c(1, 2, 3), ul = c(NA, 1, 2))
  ))
})

test_that(".calm_assert throws error for unsuported functional stimuli", {
  map <- parse_design(data.frame(
    g = "a",
    p1 = "1(A_a)(US)/1(A_b)(US)", r1 = TRUE
  ))@mapping
  expect_error(.calm_assert(
    "no_functional_stimuli", map
  ))
})
