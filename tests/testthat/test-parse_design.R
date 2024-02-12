exp_df <- data.frame(
  group = c("g1", "g2"),
  p1 = c("10A>(US)", "10A"), r1 = FALSE
)
parsed <- parse_design(exp_df)

test_that("raw_design slot exists", {
  expect_true("raw_design" %in% slotNames(parsed))
})

test_that("trials method works", {
  expect_true(nrow(trials(parsed)) > 0)
})
