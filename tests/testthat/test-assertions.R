test_that(".calmr_assert warns with NULL model", {
  expect_warning(.calmr_assert("supported_model", NULL))
})

test_that(".calmr_assert throws error with weird model", {
  expect_error(.calmr_assert("supported_model", "Weird"))
})

test_that(".calmr_assert throws error with no design", {
  expect_error(.calmr_assert("parsed_design", NULL))
})

test_that(".calmr_assert throws error with weird optimizer", {
  expect_error(.calmr_assert("supported_optimizer", "Weird"))
})

test_that(".calmr_assert throws error with weird family", {
  expect_error(.calmr_assert("supported_family", "Weird"))
})

test_that(".calmr_assert throws error for upper and lower limits with NAs", {
  expect_error(.calmr_assert(
    "limits_OK",
    list(ll = c(1, 2, 3), ul = c(NA, 1, 2))
  ))
})

test_that(".calmr_assert throws error for unsuported functional stimuli", {
  map <- parse_design(data.frame(
    g = "a",
    p1 = "1(A_a)(US)/1(A_b)(US)", r1 = TRUE
  ))@mapping
  expect_error(.calmr_assert(
    "no_functional_stimuli", map
  ))
})

test_that(".calmr_assert throws error for nonexistent folder", {
  expect_error(.calmr_assert("filepath_OK", "my_folders/none.jpg"))
})

test_that(".calmr_assert throws error for unsupported plot", {
  expect_error(.calmr_assert("supported_plot",
    letters[4],
    supported = letters[1:3]
  ))
})
