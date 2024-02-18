# TODO: Test graphs
df <- data.frame(
  Group = "X",
  P1 = "2AB(US)",
  R1 = TRUE
)
df <- parse_design(df)
models <- supported_models()

test_that("calmr_model_graph works", {
  res <- run_experiment(
    df,
    model = models[1],
    parameters = get_parameters(design = df, model = models[1]),
    options = get_exp_opts()
  )
  g <- calmr_model_graph(results(res)$vs)
  expect_named(g)
})

test_that("calmr_model_graph takes a trial", {
  res <- run_experiment(
    df,
    model = models[1],
    parameters = get_parameters(design = df, model = models[1]),
    options = get_exp_opts()
  )
  g <- calmr_model_graph(results(res)$vs, t = 1)
  expect_named(g)
})

# Test graphs for every model
for (m in models) {
  test_that(sprintf("graphs for model %s", m), {
    res <- run_experiment(
      df,
      model = m,
      parameters = get_parameters(design = df, model = m),
      options = get_exp_opts()
    )
    g <- graph(res)
    expect_named(g)
  })
}
