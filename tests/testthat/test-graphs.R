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
    parameters = get_parameters(design = df, model = models[1])
  )
  g <- calmr_model_graph(results(res)$vs)
  expect_named(g)
})

res <- run_experiment(
  df,
  model = models[1],
  parameters = get_parameters(design = df, model = models[1])
)
test_that("calmr_model_graph takes a trial", {
  g <- calmr_model_graph(results(res)$vs, t = 1)
  expect_named(g)
})

test_that("calmr_model_graph throws a warning if trial exceeds data", {
  expect_warning(calmr_model_graph(results(res)$vs, t = 5000))
})

# Test graphs for every model
for (m in models) {
  test_that(sprintf("graphs for model %s", m), {
    res <- run_experiment(
      df,
      model = m,
      parameters = get_parameters(design = df, model = m)
    )
    g <- graph(res)
    expect_named(g)
  })
}

test_that("can patch graphs", {
  g <- graph(res)
  expect_no_error(patch_graphs(c(g, g)))
})

test_that("can get large options, and they work", {
  opts <- get_graph_opts(graph_size = "large")
  expect_no_error(graph(res, options = opts))
})
