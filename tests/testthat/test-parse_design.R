exp_df <- data.frame(
  group = c("g1", "g2"),
  p1 = c("10A>(US)/10A", "10A/10B"), r1 = FALSE
)
parsed <- parse_design(exp_df)

test_that("raw_design slot exists", {
  expect_true("raw_design" %in% slotNames(parsed))
})
test_that("trials method works", {
  expect_true(nrow(trials(parsed)) > 0)
})

trans_df <- data.frame(
  group = "g1",
  p1 = "10A>B>(US)/10A>B",
  r1 = FALSE
)

parsed <- parse_design(trans_df)
test_that("transition_names are nested within trials", {
  expect_setequal(
    names(mapping(parsed)$transitions),
    mapping(parsed)$trial_names
  )
})

test_that("transitions are correctly encoded", {
  expect_setequal(mapping(parsed)$transitions$`A>B>(US)`, c("A>B", "B>(US)"))
})
