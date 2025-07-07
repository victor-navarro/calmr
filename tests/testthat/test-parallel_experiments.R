# use a sequential plan if there is a ENV variable for github actions and
# the platform is mac-os, because the github runner fails with multisession

set_plan <- function() {
  if (
    nchar(Sys.getenv("GITHUB_PAT")) > 0 &&
      Sys.info()["sysname"] == "Darwin") {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession(workers = 2))
  }
}

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
  on.exit(future::plan(future::sequential))
  set_plan()
  args <- make_experiment(df,
    model = "RW1972",
    parameters = pars,
    iterations = 2
  )
  expect_equal(length(args), 2)
})

test_that("run_experiment can be run/parsed/aggregated in parallel", {
  on.exit(future::plan(future::sequential))
  set_plan()
  exp <- run_experiment(args)
  expect_named(results(exp))
})


test_that(".parallel_standby message works", {
  on.exit(future::plan(future::sequential))
  set_plan()
  pb <- progressr::progressor(1)
  expect_silent(progressr::with_progress({
    calmr:::.parallel_standby(pb)
    for (p in 1) {
      pb()
    }
  }))
})
