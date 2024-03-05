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
  model = "HD2022", options = get_exp_opts()
)

test_that("trials are generated in blocks", {
  blocks <- rep(1:20, each = 3)
  tps <- minib_args@arguments$experience[[1]]$tp
  # should generate 20 blocks for group X
  # pointers within block sum to 6 (1+2+3)
  expect_true(all(tapply(tps, blocks, sum) == 6))

  # should generate 4 blocks for group Y
  # pointers within block sum to 28 (4+3+15+6)
  blocks <- rep(1:4, each = 8)
  tps <- minib_args@arguments$experience[[2]]$tp
  expect_true(all(tapply(tps, blocks, sum) == 28))
  #
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
opts <- get_exp_opts()
parameters <- get_parameters(df, "RW1972")

test_that("function works with even trials per row", {
  args <- make_experiment(df,
    parameters = parameters, model = "RW1972", options = opts
  )
  expect_named(args@arguments)
})

test_that("make_experiment fails with too many models", {
  expect_error(make_experimendt(df,
    parameters = parameters,
    model = c("RW1972", "MAC1975"), options = opts
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
  exp <- make_experiment(df, parameters = pars, model = "ANCCR")
  expect_true(!("p1" %in% experience(exp)[[2]]$phase))
})

# TODO: Write more tests (unique sampling per iteration, master list)
