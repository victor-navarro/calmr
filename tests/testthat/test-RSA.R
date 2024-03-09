df <- get_design("blocking")[1, ]
parse_design(df)
models <- c("HD2022", "RW1972", "PKH1982")
comp <- compare_models(df, models)

comparisons <- list(
  "HD2022" = c("vs"),
  "RW1972" = c("vs"),
  "PKH1982" = c("eivs")
)

res <- rsa(comp, comparisons = comparisons)

test_that("rsa method works with many models", {
  expect_true(is.array(res@corr_mat))
})

test_that("rsa method works with one model", {
  comparisons <- list(
    "RW1972" = model_outputs("RW1972")
  )
  res <- rsa(comp, comparisons = comparisons)
  res
  expect_true(is.array(res@corr_mat))
})

test_that("rsa method stops with models outside x", {
  expect_error(rsa(comp, comparisons = list("HDI2020" = c("os"))))
})

test_that("rsa method stops with bad model outputs", {
  expect_error(rsa(comp, comparisons = list("HD2022" = c("os"))))
})

test_res <- test(res, n_samples = 20)
test_that("test method for CalmRSA works", {
  expect_named(test_res@test_data)
})

test_that("plotting RSA works", {
  plt <- plot(res)
  expect_named(plt)
})

test_that("plotting RSA with a test works", {
  plt <- plot(test_res)
  expect_named(plt)
})
