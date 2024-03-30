df <- get_design("blocking")
parsed_df <- parse_design(df)

test_that("show method works", {
  expect_no_error(capture_message(show(parsed_df)))
})

test_that("mapping method returns the mapping", {
  map <- parsed_df@mapping
  expect_equal(map, mapping(parsed_df))
})

test_that("trial method works", {
  expect_no_error(trials(parsed_df))
})
