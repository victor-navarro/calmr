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

test_that("raises error if parameters are not set", {
  mod <- methods::new("RW1972")
  expect_error(parameters(mod))
})

test_that("can set parameters using method", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  expect_true(length(mod@parameters) > 0)
})

test_that("raises error if parameters are not a list", {
  mod <- methods::new("RW1972")
  expect_error(parameters(mod) <- "not a list")
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
  expect_error(raw_results(mod))
  mod <- run(mod, experience = exper, mapping = mapp)
  expect_true(length(raw_results(mod)) > 0)
})

test_that("raw_results method returns last results", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  mod <- run(mod, experience = exper, mapping = mapp)
  results_mod <- raw_results(mod)
  expect_true(length(results_mod) > 0)
  expect_true(all(names(results_mod) %in% mod@outputs))
})

test_that("show prints default parameters if not set", {
  mod <- methods::new("RW1972")
  expect_message(show(mod), regexp = "Default Parameters:")
})

test_that("show prints parameters if set", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  expect_message(show(mod), regexp = "Parameters:")
})

test_that("run method fails without parameters", {
  mod <- methods::new("RW1972")
  expect_error(run(mod, experience = exper, mapping = mapp))
})

to_test <- c("RW1972", "SM2007", "HD2022", "HDI2020", "MAC1975", "PKH1982")
# Note: this test assumes that the model in question
# can resume training from its current state.
for (m in to_test) {
  test_that(sprintf("model %s can resume training", m), {
    mod <- methods::new(m)
    des <- get_design("blocking")[1, 1:2]
    pars <- get_parameters(des, model = m)
    exp <- make_experiment(des, parameters = pars, model = m)
    parameters(mod) <- pars
    exper <- exp@experiences[[1]]
    mapp <- exp@design@mapping
    mod <- run(mod,
      experience = exper,
      mapping = mapp
    )

    first <- ifelse(m == "PKH1982", mod@ev["N", "US"], mod@v["N", "US"])
    expect_true(first > 0)
    # Resume training
    mod <- run(mod, experience = exper, mapping = mapp)
    second <- ifelse(m == "PKH1982", mod@ev["N", "US"], mod@v["N", "US"])
    expect_true(second > first)
  })
}

test_that("can parse results", {
  mod <- methods::new("RW1972")
  parameters(mod) <- pars
  mod <- run(mod, experience = exper, mapping = mapp)
  parsed_results <- parse(mod, outputs = c("associations", "responses"))
  expect_true(length(parsed_results) > 0)

  # but throws error if wrong outputs
  expect_error(parse(mod, outputs = "invalid_output"))

  # or when the parsing function is not defined
  mod@.parse_map <- list() # clear parse map
  expect_error(parse(mod, outputs = "associations"))
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

test_that("can deal with graphing PKH1982", {
  mod <- methods::new("PKH1982")
  parameters(mod) <- get_parameters(des, model = "PKH1982")
  mod <- run(mod, experience = exper, mapping = mapp)
  # can't graph without parsing the model
  expect_error(graph(mod, outputs = "associations"))
  # can graph after parsing
  mod <- parse(mod, outputs = c("associations"))
  g <- graph(mod)
  expect_true(length(g) > 0)
})
