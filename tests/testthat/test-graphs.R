# TODO: Test graphs
df <- data.frame(
  Group = "X",
  P1 = "2AB(US)",
  R1 = TRUE
)
df <- parse_design(df)
models <- supported_models()


test_that("calmr_model-graph works", {
  res <- run_experiment(
    df,
    model = models[1],
    parameters = get_parameters(design = df, model = m),
    options = get_exp_opts()
  )
  g <- calmr_model_graph(results(res)$vs[[1]])
  expect_named(g)
})

# Test plots for every model
for (m in models) {
  res <- run_experiment(
    df,
    model = m,
    parameters = get_parameters(design = df, model = m),
    options = get_exp_opts()
  )
  g <- graph(res)
}
