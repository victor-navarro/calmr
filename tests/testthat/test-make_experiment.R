# TODO: Write more stronger expectation tests

# A basic experiment
df <- data.frame(
  Group = c("X", "Y"),
  P1 = c("10A/10#A/10B", "2AB/6A/6C/2#AB"),
  R1 = c(TRUE, TRUE),
  P2 = c("10A/10#A/10B", "2AB/6A/6C/2#AB"),
  R2 = c(TRUE, TRUE)
)

parsed_df <- parse_design(df)
parameters <- get_parameters(df, model = "HD2022")
minib_args <- make_experiment(parsed_df,
  parameters = parameters,
  model = "HD2022", iterations = 2
)

test_that("trials are generated in blocks", {
  blocks <- rep(1:20, each = 3)
  tps <- experiences(minib_args)[[1]]$tp
  # should generate 20 blocks for group X
  # pointers within block sum to 6 (1+2+3)
  expect_true(all(tapply(tps, blocks, sum) == 6))

  # should generate 4 blocks for group Y
  # pointers within block sum to 28 (4+3+15+6)
  blocks <- rep(1:4, each = 8)
  tps <- experiences(minib_args)[[2]]$tp
  expect_true(all(tapply(tps, blocks, sum) == 28))
  #
})

test_that("no miniblocks", {
  expect_no_error(
    make_experiment(parsed_df,
      parameters = parameters,
      model = "HD2022",
      miniblocks = FALSE
    )
  )
})

test_that("no miniblocks problematic design", {
  des <- get_design("blocking")
  expect_no_error(
    make_experiment(des,
      parameters = get_parameters(des, model = "HD2022"),
      model = "HD2022",
      miniblocks = FALSE
    )
  )
})

# More tests
df <- data.frame(
  Group = c("A", "B"),
  P1 = c("2A>(US)", "2B>(US)"),
  R1 = c(TRUE, TRUE),
  P2 = c("2AX>(US)", "2AX>(US)"),
  R2 = c(TRUE, TRUE)
)
df <- parse_design(df)
parameters <- get_parameters(df, "RW1972")

test_that("function works with even trials per row", {
  args <- make_experiment(df,
    parameters = parameters, model = "RW1972"
  )
  expect_true(length(args) > 1)
})

test_that("make_experiment fails with too many models", {
  expect_error(make_experiment(df,
    parameters = parameters,
    model = c("RW1972", "MAC1975")
  ))
})

# A problematic design
df <- data.frame(
  group = c("Blocking", "Control"),
  p1 = c("10N>(US)", ""), r1 = FALSE,
  p2 = c("10NL>(US)", "10NL>(US)/10#L"), r2 = FALSE
)
pars <- get_parameters(df, model = "ANCCR")

test_that("can make an experiment with empty phases", {
  exp <- make_experiment(df,
    parameters = pars, timings = get_timings(df, "ANCCR"),
    model = "ANCCR"
  )
  expect_true(!("p1" %in% experiences(exp)[[2]]$phase))
})
