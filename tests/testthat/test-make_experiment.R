# TODO: Write more stronger expectation tests

# A basic experiment
df <- data.frame(
  Group = c("X", "Y"),
  P1 = c("!10A/10#A/10B", "!2AB/6A/6C/2#AB"),
  P2 = c("!10A/10#A/10B", "!2AB/6A/6C/2#AB")
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
  P1 = c("!2A>(US)", "!2B>(US)"),
  P2 = c("!2AX>(US)", "!2AX>(US)")
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
  p1 = c("10N>(US)", ""),
  p2 = c("10NL>(US)", "10NL>(US)/10#L")
)
pars <- get_parameters(df, model = "ANCCR")
tims <- timings <- get_timings(df, "ANCCR")

test_that("can make an experiment with empty phases", {
  exp <- make_experiment(df,
    parameters = pars, timings = tims,
    model = "ANCCR"
  )
  expect_true(!("p1" %in% experiences(exp)[[2]]$phase))
})

test_that("seeding works", {
  nonseeded <- make_experiment(df,
    parameters = pars, timings = tims,
    model = "ANCCR"
  )
  seed1 <- make_experiment(df,
    parameters = pars, timings = tims,
    model = "ANCCR", seed = 123
  )
  # to test the state of the RNG is OK after
  # running a seeded generation
  rand1 <- rnorm(100)
  seed2 <- make_experiment(df,
    parameters = pars, timings = tims,
    model = "ANCCR", seed = 123
  )
  rand2 <- rnorm(100)
  expect_true(any(experiences(nonseeded)[[1]]$time !=
    experiences(seed1)[[1]]$time))
  expect_true(all(experiences(seed1)[[1]]$time ==
    experiences(seed2)[[1]]$time))
  expect_true(any(rand1 != rand2))
  expect_true(seed1@.seed == seed2@.seed)
})
