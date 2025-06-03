test_that("softmax function .soft works", {
  expect_equal(.5, .soft(c(1, 1))[1])
})


test_that(".gen_ss_weights works", {
  mat <- .gen_ss_weights(c("A", "B", "C"), default_val = 0.5)
  expect_equal(mat["A", "B"], 0.5)
  expect_true(all(mat[] == 0.5))
  expect_equal(rownames(mat), c("A", "B", "C"))
  expect_equal(colnames(mat), rownames(mat))
})

test_that(".expand_ss_weights works", {
  mat <- .gen_ss_weights(c("A", "B"), default_val = 0.5)
  expanded_mat <- .expand_ss_weights(mat, c("A", "B", "C"), default_val = 0.1)
  expect_equal(expanded_mat["A", "C"], 0.1) # should have new default value
  expect_equal(expanded_mat["B", "A"], 0.5) # should keep old value
  expect_equal(rownames(expanded_mat), c("A", "B", "C"))
  expect_equal(colnames(expanded_mat), rownames(expanded_mat))
})
