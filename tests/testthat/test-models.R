df <- data.frame(
  Group = c("A", "B"),
  P1 = c("2A>(US)", "2B>(US)"),
  R1 = c(TRUE, TRUE),
  P2 = c("2AX>(US)", "2AX>(US)"),
  R2 = c(TRUE, TRUE)
)
df <- parse_design(df)
opts <- get_exp_opts()
supported_mods <- supported_models()

# test_that("get_model fails if model is not supported", {
#   expect_error(get_model(model_name = "NAVARRO"))
# })


for (m in supported_mods) {
  test_that(paste("model", m, "works"), {
    pars <- get_parameters(df, model = m)
    args <- make_experiment(df, parameters = pars, model = m, options = opts)
    res <- run_experiment(args)
    expect_named(res@results@aggregated_results)
  })
}
