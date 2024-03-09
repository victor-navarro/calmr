### Disabled due to slowness ###

# df <- data.frame(
#   Group = c("True"),
#   P1 = c("100AB(US)/100AC"),
#   R1 = TRUE,
#   P2 = "1#A",
#   R2 = TRUE
# )

# pars <- get_parameters(df, model = "RW1972")
# test_that("make_experiment can be run in parallel", {
#   future::plan(future::multisession)
#   on.exit(future::plan(future::sequential))
#   args <- make_experiment(df,
#     model = "RW1972",
#     parameters = pars,
#     iterations = 10
#   )
#   expect_equal(length(args), 10)
# })

# test_that("run_experiment can be run/parsed/aggregated in parallel", {
#   on.exit(future::plan(future::sequential))
#   args <- make_experiment(df,
#     model = "RW1972",
#     parameters = pars,
#     iterations = 10
#   )
#   future::plan(future::multisession)
#   exp <- run_experiment(args)
#   expect_named(results(exp))
# })
