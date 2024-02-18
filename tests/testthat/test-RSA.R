exp <- data.frame(
  Group = c("A", "B"),
  P1 = c("2(A)>(US)/1B>(US)", "1(A)>(US)/2B>(US)"),
  R1 = TRUE
)
exp <- parse_design(exp)
models <- c("HD2022", "RW1972", "PKH1982")
parameters <- sapply(models, get_parameters, design = exp)
options <- get_exp_opts()
exp_res <- compare_models(exp,
  models = models,
  parameters = parameters, options = options
)
comparisons <- list(
  "HD2022" = c("vs"),
  "RW1972" = c("vs"),
  "PKH1982" = c("eivs")
)

res <- rsa(exp_res, comparisons = comparisons)
res

test_that("rsa method works with many models", {
  expect_true(is.array(res@corr_mat))
})

test_that("rsa method works with one model", {
  comparisons <- list(
    "HD2022" = model_outputs("HD2022")
  )
  res <- rsa(exp_res, comparisons = comparisons)
  res
  expect_true(is.array(res@corr_mat))
})

test_that("rsa method stops with models outside x", {
  expect_error(rsa(exp_res, comparisons = list("HDI2020" = c("os"))))
})

test_that("rsa method stops with bad model outputs", {
  expect_error(rsa(exp_res, comparisons = list("HD2022" = c("os"))))
})

test_res <- test(res, n_samples = 20)
test_that("test method for CalmrRSA works", {
  expect_named(test_res@test_data)
})

test_that("plotting RSA works", {
  plt <- plot(res)
  expect_named(plt)
})

test_that("plotting RSA with a test works", {
  plt <- plot(res)
  expect_named(plt)
})
