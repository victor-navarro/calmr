df <- data.frame(
  Group = "X",
  P1 = "2AB(US)",
  R1 = TRUE
)
df <- parse_design(df)
models <- supported_models()

# Test plots for every model
for (m in models) {
  ps <- supported_plots(m)
  res <- run_experiment(
    df,
    model = m,
    parameters = get_parameters(design = df, model = m),
    options = get_exp_opts()
  )
  test_that(sprintf("all plots for model %s", m), {
    plots <- plot(res)
    expect_named(plots)
  })
  test_that(sprintf("specific plot for model %s", m), {
    p <- plot(res, type = sample(ps, 1))
    expect_true(length(p[[1]]) == 1)
  })
}

# Test that patch plot works
plots <- plot(res)
pnames <- unlist(unname(lapply(plots, names)))
test_that("patch_plot works with names", {
  expect_named(patch_plots(plots, pnames))
})
test_that("patch_plot works with numbers", {
  expect_named(patch_plots(plots, c(1, 1)))
})
test_that("patch_plot works with singles", {
  expect_named(patch_plots(plots, pnames[1]))
  expect_named(patch_plots(plots, 2))
})
test_that("patch_plot throws error with bad names", {
  expect_error(patch_plots(plots, c("bad_name", "my_plot")))
})
test_that("patch_plot throws error with bad numbers", {
  expect_error(patch_plots(plots, -3:-2))
})
