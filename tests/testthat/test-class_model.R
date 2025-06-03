# get a simple experiment
des <- get_design("blocking")[1, ]
pars <- get_parameters(des, model = "RW1972")
# make experiment
exp <- make_experiment(des, parameters = pars, model = "RW1972")
exper <- exp@experiences[[1]]
mapp <- exp@design@mapping


test_that("new model has no parameters", {
  mod <- methods::new("RW1972")
  expect_equal(length(mod@parameters), 0)
})

test_that("can set parameters using method", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  expect_true(length(mod@parameters) > 0)
})

test_that("can get parameters using method", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  expect_equal(parameters(mod), pars)
})

test_that("cannot set parameters with incorrect names", {
  mod <- methods::new("RW1972")
  expect_error(parameters(mod) <- c(pars, list(invalid_param = 1)))
})

test_that("can run model with parameters", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  mod <- run(mod, experience = exper, mapping = mapp)
  expect_true(length(mod@.last_raw_results) > 0)
})

test_that("raw_results method returns last results", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  mod <- run(mod, experience = exper, mapping = mapp)
  results_mod <- raw_results(mod)
  expect_true(length(results_mod) > 0)
  expect_true(all(names(results_mod) %in% mod@outputs))
})

test_that("run method fails without parameters", {
  mod <- methods::new("RW1972")
  expect_error(run(mod, experience = exper, mapping = mapp))
})


# TODO: Make this a comprehensive test for all models
test_that("can resume training", {
  # Note: this test assumes that the model in question
  # can resume training from its current state.
  mod <- methods::new("RW1972")
  des <- get_design("blocking")[1, 1:2]
  pars <- get_parameters(des, model = "RW1972")
  exp <- make_experiment(des, parameters = pars, model = "RW1972")
  parameters(mod) <- pars
  exper <- exp@experiences[[1]]
  mapp <- exp@design@mapping
  mod <- run(mod,
    experience = exper,
    mapping = mapp
  )
  first <- mod@v["N", "US"]
  expect_true(first > 0)
  # Resume training
  mod <- run(mod, experience = exper, mapping = mapp)
  second <- mod@v["N", "US"]
  expect_true(second > first)

  # same with SM2007
  mod <- methods::new("SM2007")
  des <- get_design("blocking")[1, 1:2]
  pars <- get_parameters(des, model = "SM2007")
  exp <- make_experiment(des, parameters = pars, model = "SM2007")
  parameters(mod) <- pars
  exper <- exp@experiences[[1]]
  mapp <- exp@design@mapping
  mod <- run(mod,
    experience = exper,
    mapping = mapp
  )
  first <- mod@v["N", "US"]
  expect_true(first > 0)
  # Resume training
  mod <- run(mod, experience = exper, mapping = mapp)
  second <- mod@v["N", "US"]
  expect_true(second > first)
})

test_that("can parse results", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  mod <- run(mod, experience = exper, mapping = mapp)
  parsed_results <- parse(mod, outputs = c("associations", "responses"))
  expect_true(length(parsed_results) > 0)
})

test_that("can plot results", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  mod <- run(mod, experience = exper, mapping = mapp)
  # can't plot without parsing the model
  expect_error(plot(mod, outputs = "associations"))
  # can plot after parsing
  mod <- parse(mod, outputs = c("associations"))
  expect_true(length(plot(mod, type = "associations")) > 0)
  # can't plot without some parsed results
  expect_error(plot(mod, outputs = "responses"))
})

test_that("can graph results", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  mod <- run(mod, experience = exper, mapping = mapp)
  # can't graph without parsing the model
  expect_error(graph(mod, outputs = "associations"))
  # can graph after parsing
  mod <- parse(mod, outputs = c("associations"))
  g <- graph(mod)
  expect_true(length(g) > 0)
})
