df <- data.frame(g = "g", p1 = "!3A>(US)")
pars <- get_parameters(df, model = "RW1972")
exper <- make_experiment(df, parameters = pars, model = "RW1972")
res <- run_experiment(exper)
responses <- results(res)$responses$value

# define model function
model_fun <- function(p, ex) {
  np <- parameters(ex)
  np[[1]]$alphas[] <- p
  parameters(ex) <- np
  results(run_experiment(ex))$responses$value
}

def_opts <- list(
  model_pars = names(pars$alphas),
  ll = rep(.1, 2), ul = rep(.9, 2)
)

test_that("get_optimizer_opts throws warning if no family was passed", {
  expect_warning(do.call(get_optimizer_opts, c(def_opts, optimizer = "optim")))
})

test_that("get_optimizer_opts throws warning if no optimizer was passed", {
  expect_warning(do.call(get_optimizer_opts, c(def_opts, family = "identity")))
})

optim_opts <- do.call(get_optimizer_opts, c(def_opts,
  optimizer = "optim", family = "normal"
))
optim_opts$initial_pars[] <- rep(.6, 3)

test_that("can fit with optim and print verbosity", {
  optim_opts$verbose <- TRUE
  expect_no_error(capture_message(fit_model(responses, model_fun, optim_opts,
    ex = exper, method = "L-BFGS-B", control = list(maxit = 1)
  )))
})

# test extra families
pois_opts <- do.call(get_optimizer_opts, c(def_opts,
  optimizer = "optim", family = "poisson"
))
pois_opts$initial_pars[] <- rep(.6, 3)

test_that("can fit poisson and can create fit file", {
  expect_no_error(fit_model(ceiling(responses * 10), model_fun, pois_opts,
    ex = exper, method = "L-BFGS-B", control = list(maxit = 1),
    file = "pois_test.rds"
  ))
})

test_that("can load a fit from file", {
  on.exit(file.remove("pois_test.rds"))
  expect_no_error(fit_model(ceiling(responses * 10), model_fun, pois_opts,
    ex = exper, method = "L-BFGS-B", control = list(maxit = 1),
    file = "pois_test.rds"
  ))
})

ga_opts <- do.call(get_optimizer_opts, c(def_opts,
  optimizer = "ga", family = "identity"
))
test_that("can fit with GA", {
  expect_no_error(fit_model(responses, model_fun, ga_opts,
    ex = exper, maxiter = 1, monitor = FALSE
  ))
})

# method tests
opt <- fit_model(responses, model_fun, optim_opts,
  ex = exper, method = "L-BFGS-B", control = list(maxit = 1)
)

test_that("show method works", {
  expect_no_error(capture_message(show(opt)))
})

test_that("NLL method works", {
  expect_no_error(NLL(opt))
})

test_that("AIC method works", {
  expect_no_error(AIC(opt))
})

test_that("BIC method works", {
  expect_no_error(BIC(opt))
})

test_that("predict method works", {
  expect_equal(length(responses), length(predict(opt, ex = exper)))
})

test_that("show method works", {
  expect_no_error(capture_message(show(opt)))
})
