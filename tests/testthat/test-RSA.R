df <- get_design("blocking")[1, ]
parse_design(df)
models <- c("HD2022", "RW1972", "PKH1982")
comp <- compare_models(df, models)

comparisons <- list(
  "HD2022" = c("associations"),
  "RW1972" = c("associations"),
  "PKH1982" = c("associations")
)

# TODO: There is a bug when one includes
# more than one output for a model and tehre are multiple models

test_that("rsa fails if comparisons is unnamed", {
  expect_error(rsa(comp, unname(comparisons)))
})

test_that("rsa fails with inconsistent model outputs", {
  expect_error(rsa(comp, list("HD2022" = c("pools", "associations"))))
})

test_that("rsa can be ran with test = TRUE", {
  expect_no_error(rsa(comp, comparisons, test = TRUE))
})

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
  expect_error(rsa(comp,
    comparisons = list("HDI2020" = c("operator_switches"))
  ))
})

test_that("rsa method stops with bad model outputs", {
  expect_error(rsa(comp, comparisons = list("HD2022" = c("operator_switches"))))
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
  plt <- plot(test_res)
  expect_named(plt)
})

test_that("show method works without test results", {
  expect_no_error(capture_message(show(res)))
})

test_that("show method works with test results", {
  expect_no_error(capture_message(show(test_res)))
})

comparisons <- list(
  "RW1972" = c("responses", "associations")
)

test_that("can do rsa for multiple outputs of a model", {
  expect_no_error(
    rsa(comp, comparisons)
  )
})
