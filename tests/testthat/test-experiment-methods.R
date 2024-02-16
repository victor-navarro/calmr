df <- df <- data.frame(
  Group = c("True", "Pseudo"),
  P1 = c("2AB(US)/2AC", "1AB(US)/1AB/1AC(US)/1AC"),
  R1 = c(TRUE, TRUE),
  P2 = c("1#A", "1#A"),
  R2 = c(TRUE, TRUE)
)
models <- c("RW1972", "MAC1975")
df <- parse_design(df)

experiments <- sapply(
  models,
  function(m) {
    make_experiment(df,
      model = m,
      parameters = get_parameters(df, model = m),
      options = get_exp_opts()
    )
  },
  simplify = FALSE, USE.NAMES = FALSE
)

ran_experiments <- lapply(experiments, run_experiment)

concat <- do.call(c, experiments)
# Concatenation tests
test_that("c method works with make_experiment", {
  expect_named(concat@arguments)
})

concat <- do.call(c, ran_experiments)
test_that("c concatenates aggregated_results", {
  base_length <- length(ran_experiments[[1]]@results@aggregated_results)
  expect_true(length(concat@results@aggregated_results) > base_length)
})
test_that("c concatenates parsed_results", {
  base_length <- length(ran_experiments[[1]]@results@parsed_results)
  expect_true(length(concat@results@parsed_results) > base_length)
})
test_that("c concatenates raw_results", {
  base_length <- length(ran_experiments[[1]]@results@raw_results)
  expect_true(length(concat@results@raw_results) > base_length)
})
