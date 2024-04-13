exp_df <- get_design("blocking")
parsed <- parse_design(exp_df)



test_that("raw_design slot exists", {
  expect_true("raw_design" %in% slotNames(parsed))
})

test_that("trials method works", {
  expect_true(nrow(trials(parsed)) > 0)
})

trans_df <- data.frame(
  group = "g1",
  p1 = "10A>B>(US)/10AB"
)
parsed <- parse_design(trans_df)
test_that("transition_names are nested within trials", {
  expect_true(
    all(
      names(mapping(parsed)$transitions) %in%
        mapping(parsed)$trial_names
    )
  )
})

test_that("transitions are correctly encoded", {
  expect_setequal(mapping(parsed)$transitions$`A>B>(US)`, c("A>B", "B>(US)"))
})

test_that("no transitions for trials with no transitions", {
  expect_true(is.null(mapping(parsed)$transitions[["10AB"]]))
})

# A problematic design
df <- data.frame(
  group = c("Blocking", "Control"),
  p1 = c("10N>(US)", ""),
  p2 = c("10NL>(US)", "10NL>(US)/10#L")
)

test_that("no transitions for trial in design with empty phases", {
  expect_true(!("#L" %in% parse_design(df)@mapping$transitions))
})
