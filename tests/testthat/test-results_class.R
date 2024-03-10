df <- get_design("blocking")
exp <- run_experiment(df,
  model = "RW1972",
  parameters = get_parameters(df, model = "RW1972")
)
noagg <- run_experiment(df,
  model = "RW1972",
  parameters = get_parameters(df, model = "RW1972"),
  aggregate = FALSE
)

test_that("show method works with aggregated_results", {
  expect_no_error(capture_message(show(exp@results)))
})

test_that("show method works with sans aggregated_results", {
  expect_no_error(capture_message(show(noagg@results)))
})
