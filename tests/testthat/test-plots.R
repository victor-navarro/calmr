df = data.frame(Group = "X",
                P1 = "1AB(US)",
                R1 = TRUE)

models = supported_models()
for (m in models){
  ps = supported_plots(m)
  mod = quick_model(design_df = df,
                    model = m,
                    param_df = get_params(design = df, model = m))
  for (p in ps){
    test_that(paste("plot", p, "for model", m, "works"), {
      expect_type(calmr_plot(mod, p), "list")
    })
  }
}
