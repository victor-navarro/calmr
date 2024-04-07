test_that("set_calmr_palette returns available palettes", {
  expect_no_error(set_calmr_palette())
})

test_that("set_calmr_palette actually sets global option", {
  on.exit(options("calmr_palette" = NULL))
  set_calmr_palette("hue")
  expect_equal(getOption("calmr_palette"), "hue")
})

test_that(".calmr_scales returns a ggplot scale", {
  expect_true(inherits(.calmr_scales("colour_d"), "gg"))
})

test_that("set_calmr_palette has an effect on .calmr_scales", {
  on.exit(options("calmr_palette" = NULL))
  expect_true(inherits(.calmr_scales("colour_d"), "gg"))
  set_calmr_palette("hue")
  expect_true(inherits(.calmr_scales("colour_d"), "gg"))
})

test_that("set_calmr_palette throws error with weird palette", {
  on.exit(options("calmr_palette" = NULL))
  expect_error(set_calmr_palette("huehue"))
})
