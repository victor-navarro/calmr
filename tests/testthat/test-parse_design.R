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

# A design with the old format for randomization
old_rand_df <- data.frame(
  group = c("A", "B"),
  p1 = c("10AB/10C", "10AC/10A"),
  r1 = c(TRUE, FALSE),
  p2 = c("10A/10B", "10B/10C"),
  r2 = c(FALSE, TRUE)
)

test_that("compatibility function works", {
  compat_rand_df <- suppressWarnings(calmr:::.design_compat(old_rand_df))

  # the randomization columns should be removed
  expect_false(any(grepl("r", names(compat_rand_df[2:ncol(compat_rand_df)]))))
  # get number of randomization entries
  n_rand <- sum(sapply(old_rand_df, class) == "logical")

  # get entries which should have a !
  targets <- which(old_rand_df == TRUE) - (1:n_rand) * nrow(old_rand_df)
  expect_true(all(grepl("!", unlist(compat_rand_df)[targets])))
})

test_that("warning is thrown for old randomization format", {
  expect_warning(
    parse_design(old_rand_df)
  )
})
