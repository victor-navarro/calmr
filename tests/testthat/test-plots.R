df <- data.frame(
  Group = "X",
  P1 = "!2A(US)"
)
df <- parse_design(df)
models <- supported_models()

# Test plots for every model
for (m in models) {
  if (m %in% supported_timed_models()) {
    tims <- get_timings(df, model = m)
  } else {
    tims <- NULL
  }
  ps <- supported_plots(m)
  res <- run_experiment(
    df,
    model = m,
    timings = tims,
    parameters = get_parameters(design = df, model = m)
  )
  test_that(sprintf("all plots for model %s", m), {
    plots <- plot(res)
    expect_named(plots)
  })
  test_that(sprintf("specific plot for model %s", m), {
    p <- plot(res, type = sample(ps, 1))
    expect_true(length(p) == 1)
  })
}

# Test that patch plot works
plots <- plot(res)
pnames <- names(plots)
test_that("patch_plot works with names", {
  expect_true(inherits(patch_plots(plots, pnames), "patchwork"))
})
test_that("patch_plot works with numbers", {
  expect_true(inherits(patch_plots(plots, c(1, 1)), "patchwork"))
})
test_that("patch_plot works with singles", {
  expect_true(inherits(patch_plots(plots, pnames[1]), "patchwork"))
  expect_true(inherits(patch_plots(plots, 2), "patchwork"))
})
test_that("patch_plot throws error with bad names", {
  expect_error(patch_plots(plots, c("bad_name", "my_plot")))
})
test_that("patch_plot throws error with bad numbers", {
  expect_error(patch_plots(plots, -3:-2))
})


test_that("can get default scales", {
  default_scales <- c("colour_d", "colour_c", "fill_d", "fill_c")
  for (s in default_scales) {
    expect_no_error(.calmr_scales(s))
  }
})
