test_that("throws error with weird model", {
  expect_error(.assert_model("Weird"))
})

# this assertion is a placeholder for the future
test_that("throws error with weird time model", {
  expect_error(.assert_timed_model("Weird"))
})
# this assertion is a placeholder for the future
test_that("is fine with time model", {
  expect_no_error(.assert_timed_model("TD"))
})


test_that("throws warning with no timings for a time-based model", {
  expect_warning(make_experiment(get_design("blocking"),
    model = "TD",
    parameters = get_parameters(get_design("blocking"), model = "TD")
  ))
})

test_that("throws error with weird optimizer", {
  expect_error(.assert_optimizer("Weird"))
})

test_that("throws error with weird family", {
  expect_error(.calmr_assert("supported_family", "Weird"))
})

test_that("throws error for upper and lower limits with NAs", {
  expect_error(.assert_limits(
    list(ll = c(1, 2, 3), ul = c(NA, 1, 2))
  ))
})

test_that("throws error for unsuported functional stimuli", {
  map <- parse_design(data.frame(
    g = "a",
    p1 = "!1(A_a)(US)/1(A_b)(US)"
  ))@mapping
  expect_error(.assert_no_functional(map))
})

test_that("sthrows error for nonexistent folder", {
  expect_error(.assert_filepath("my_folders/none.jpg"))
})

test_that(".sanitize_outputs returns all outputs if outputs are null", {
  expect_setequal(model_outputs("ANCCR"), .sanitize_outputs(NULL, "ANCCR"))
})

test_that(".sanitize_outputs throws warning for extra outputs", {
  expect_warning(.sanitize_outputs(c("associations", "activations"), "RW1972"))
})

test_that(".sanitize_outputs does not add extra outputs", {
  expect_setequal(
    .sanitize_outputs(c("associations"), "RW1972"),
    "associations"
  )
})
