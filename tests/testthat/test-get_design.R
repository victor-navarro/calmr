test_that("get_design returns all designs", {
  expect_named(get_design())
})

test_that("get_design always returns data.frames", {
  des <- names(get_design())
  for (d in des) {
    expect_true(nrow(get_design(d)) > 1)
  }
})
