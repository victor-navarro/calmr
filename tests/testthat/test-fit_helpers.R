test_that(".get_calmr_link throws error with incorrect family", {
  expect_error(.get_calmr_link("abnormal"))
})

test_that(".get_calmr_loglikelihood throws error with incorrect family", {
  expect_error(.get_calmr_loglikelihood("abnormal"))
})
