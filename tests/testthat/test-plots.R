df <- data.frame(
  Group = "X",
  P1 = "1AB(US)",
  R1 = TRUE
)

models <- supported_models()
for (m in models) {
  ps <- supported_plots(m)
  mod <- run_experiment(
    design = df,
    model = m,
    param_df = get_parameters(design = df, model = m),
    options = get_exp_opts()
  )
  for (p in ps) {
    test_that(paste("plot", p, "for model", m, "works"), {
      expect_type(plot(mod, type = p), "list")
    })
  }
}
