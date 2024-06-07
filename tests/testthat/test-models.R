df <- data.frame(
  Group = c("A", "B"),
  P1 = c("!2A>(US)", "!2B>(US)"),
  P2 = c("!2AX>(US)", "!2AX>(US)")
)
df <- parse_design(df)
supported_mods <- supported_models()

test_that("get_model fails if model is not supported", {
  expect_error(get_model(model_name = "NAVARRO"))
})

for (m in supported_mods) {
  test_that(paste("model", m, "works"), {
    if (m %in% supported_timed_models()) {
      tims <- get_timings(df, model = m)
    } else {
      tims <- NULL
    }
    pars <- get_parameters(df, model = m)
    args <- make_experiment(df,
      parameters = pars,
      timings = tims,
      model = m
    )
    res <- run_experiment(args)
    expect_named(res@results@aggregated_results)
  })
}
