df <- data.frame(
  Group = c("A", "B"),
  P1 = c("10A>(US)", "10B>(US)"),
  R1 = c(TRUE, TRUE),
  P2 = c("10AX>(US)", "10AX>(US)"),
  R2 = c(TRUE, TRUE)
)
df <- parse_design(df)

test_that("get_model fails if model is not supported", {
  expect_error(get_model(model_name = "NAVARRO"))
})

supported_mods <- supported_models()
for (m in supported_mods) {
  test_that(paste("model", m, "works"), {
    expect_true(class(run_model(
      make_model_args(df, get_parameters(df, model = m), model = m),
      parse = F
    )) == "CalmrExperiment")
  })
}
