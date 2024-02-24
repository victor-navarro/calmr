rawdf <- data.frame(
  group = "G",
  p1 = c("10A>(US)/10B>(US)/10(US)"),
  r1 = TRUE
)
augment_args <- list(reward_labels = "US")

test_that("you can augment a design after parsing", {
  df <- parse_design(rawdf)
  expect_true(augment(df, model = "ANCCR", augment_args)@augmented)
})

test_that("you can augment a design as you parse", {
  expect_true(parse_design(rawdf, model = "ANCCR", augment_args)@augmented)
})
