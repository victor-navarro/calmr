# A basic experiment
df <- data.frame(
  Group = c("X", "Y"),
  P1 = c("10A/#10A/10B", "2AB/6A/6C/2#AB"),
  R1 = c(TRUE, TRUE),
  P2 = c("10A/#10A/10B", "2AB/6A/6C/2#AB"),
  R2 = c(TRUE, TRUE)
)

parsed_df <- parse_design(df)
parameters <- get_parameters(df, model = "HD2022")

no_minib_args <- make_experiment(parsed_df, parameters,
  model = "HD2022", options = get_exp_opts(miniblocks = FALSE, iterations = 2)
)

# WORKING HERE

minib_args <- make_model_args(parsed_df, parameters,
  model = "HD2022",
  options = get_exp_opts(miniblocks = TRUE)
)

test_that("make_experiment works", {
  # should generate 10 blocks for group X,
  # each containing one of each trial type
  expect_true(all(sapply(1:3, function(x) {
    sum(ceiling(which(minib_args$experience[[1]]$tp == x) / 3)) == sum(1:10)
  })))
  # should generate 2 blocks for group Y,
  # each containing 1 of each AB and #AB trials, and 3 each of A and B trials
  expect_true(all(sapply(4:5, function(x) {
    sum(ceiling(which(minib_args$experience[[2]]$tp == x) / 8))
  }) == sum(1:2)))
  expect_true(all(sapply(c(1, 3), function(x) {
    sum(ceiling(which(minib_args$experience[[2]]$tp == x) / 8))
  }) == sum(rep(1:2, 3))))
  # should generate 10 blocks for group X,
  # in repeating order (1, 2, 3, 1, 2, 3, etc.)
  expect_equal(minib_args$experience[[1]]$tp, rep(1:3, 10))
  # should generate 2 blocks for group Y,
  # in repeating order (4, 1, 1, 1, 3, 3, 5, and so on)
  expect_equal(
    minib_args$experience[[2]]$tp,
    rep(c(4, 1, 1, 1, 3, 3, 3, 5), 2)
  )
})
