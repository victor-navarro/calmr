test_that("set_calmr_palette returns available palettes", {
  expect_no_error(set_calmr_palette())
})

test_that("set_calmr_palette actually sets global option", {
  on.exit(options("calmr_palette" = NULL))
  set_calmr_palette("hue")
  expect_equal(getOption("calmr_palette"), "hue")
})

test_that("set_calmr_palette has an effect on .calmr_scales", {
  on.exit(options("calmr_palette" = NULL))
  expect_equal(.calmr_scales("colour_d")$scale_name, "viridis_d")
  set_calmr_palette("hue")
  expect_equal(.calmr_scales("colour_d")$scale_name, "hue")
})

test_that("set_calmr_palette throws error with weird palette", {
  on.exit(options("calmr_palette" = NULL))
  expect_error(set_calmr_palette("huehue"))
})
