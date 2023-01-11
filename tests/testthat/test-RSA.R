exp <- data.frame(Group = c("A", "B"),
                  P1 = c("10(A_a)>(US)/5B>(US)", "5(A_b)>(US)/10B>(US)"),
                  R1 = T)
models <- c("HD2022", "HDI2020", "RW1972")
comparison <- c("rs", "rs", "vs")
comp <- suppressWarnings(compare_models(exp, models, comparison))
rsa <- RSA(comp)@corr_mat
rsatest <- RSATest(comp, n_samples = 2)


test_that("RSA works", {
  expect_true(is.array(rsa))
})

test_that("RSATest works", {
  expect_true(is.array(rsatest@sig_mat))
})

test_that("plotting RSA works", {
  expect_true("gg" %in% class(plot(RSA(comp))))
})

test_that("plotting RSATest works", {
  expect_true("gg" %in% class(plot(rsatest)))
})
