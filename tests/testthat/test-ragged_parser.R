# write some tests to test that the parser for ragged data
# ragged data is defined as lists with arrays that vary in dimensionality
# e.g., HeiDI's pools

df <- data.frame(group = "p", p1 = "1A>(US)/1A/2AB")
res <- run_experiment(df,
  model = "HD2022",
  parameters = get_parameters(df, model = "HD2022")
)
ps <- results(res)$pools

test_that("combvs in HeiDI both contain correct compound names", {
  expect_equal(
    with(ps[ps$type == "combvs", ], s1),
    c(rep(c("A,US", "A"), each = 3), rep("A,B", 6))
  )
})

test_that("chainvs have the correct dimentionality", {
  expect_equal(
    unname(c(with(
      ps[ps$type == "chainvs"],
      tapply(value, trial_type, length)
    ))),
    c(3, 6, 12)
  ) # 1, 2, and 2 roots in A, A>(US), and AB trials, but AB trials are x2
})

test_that("chainvs have the correct dimensionality across trials", {
  expect_equal(
    unname(c(with(
      ps[ps$type == "chainvs"],
      tapply(value, trial, length)
    ))),
    c(6, 3, 6, 6) # note how the ordering is different from the above
    # obeys trial ordering instead of alphanumerical ordering by trial_type
  )
})
