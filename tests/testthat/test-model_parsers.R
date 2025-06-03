# tests the model parsing functions
# some parsing functions need direct testing because
# codecov is not able to detect them within the S4 classes

des <- get_design("blocking")[1, ]


# PKH1982 covers three cases
pars <- get_parameters(des, model = "PKH1982")
exp <- make_experiment(des, parameters = pars, model = "PKH1982")
exp <- run_experiment(exp)
mod <- exp@models[[1]]

test_that(".parse_nd works", {
  expect_no_error(.parse_nd(mod, "responses"))
})

test_that(".parse_typed works", {
  expect_no_error(.parse_typed(mod, "associations"))
})

test_that(".parse_2d works", {
  expect_no_error(.parse_2d(mod, "associabilities"))
})

# HD2022 covers one case
pars <- get_parameters(des, model = "HD2022")
exp <- make_experiment(des, parameters = pars, model = "HD2022")
exp <- run_experiment(exp)
mod <- exp@models[[1]]
test_that(".parse_typed_ragged works", {
  expect_no_error(.parse_typed_ragged(mod, "pools"))
})

# TD covers one case
pars <- get_parameters(des, model = "TD")
tims <- get_timings(des, model = "TD")
exp <- make_experiment(des, parameters = pars, timings = tims, model = "TD")
exp <- run_experiment(exp)
mod <- exp@models[[1]]

test_that(".parse_nested_ragged works", {
  expect_no_error(.parse_nested_ragged(mod, "associations"))
})
