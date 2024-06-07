## Disabled due to slowness ###

df <- data.frame(
  Group = c("True"),
  P1 = c("!2AB(US)/2AC")
)
pars <- get_parameters(df, model = "RW1972")

args <- make_experiment(df,
  model = "RW1972",
  parameters = pars,
  iterations = 2
)

test_that("make_experiment can be run in parallel", {
  future::plan(future::multisession)
  on.exit(future::plan(future::sequential))
  args <- make_experiment(df,
    model = "RW1972",
    parameters = pars,
    iterations = 2
  )
  expect_equal(length(args), 2)
})

test_that("run_experiment can be run/parsed/aggregated in parallel", {
  on.exit(future::plan(future::sequential))
  future::plan(future::multisession)
  exp <- run_experiment(args)
  expect_named(results(exp))
})


test_that(".parallel_standby message works", {
  on.exit({
    future::plan(future::sequential)
  })
  future::plan(future::multisession)
  pb <- progressr::progressor(1)
  expect_silent(progressr::with_progress({
    calmr:::.parallel_standby(pb)
    for (p in 1) {
      pb()
    }
  }))
})
