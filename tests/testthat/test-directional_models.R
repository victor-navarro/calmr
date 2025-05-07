# Test that directional models do not form reciprocal associations
exp <- data.frame(group = "G", phase1 = c("10A>B"))
models_to_test <- c("RW1972", "MAC1975", "PKH1982") # Current directional models

for (model in models_to_test) {
  test_that(sprintf("model %s does not form reciprocal associations", model), {
    pars <- get_parameters(exp, model = model)
    res <- results(run_experiment(exp,
      model = model,
      parameters = pars
    ))$associations
    expect_true(all(res$value[res$s1 == "B" & res$s2 == "A"] == 0))
  })
}

# Test that directional models can cope with more three periods
exp <- data.frame(group = "G", phase1 = c("10A>B>C"))
for (model in models_to_test) {
  test_that(sprintf("model %s copes with three periods", model), {
    pars <- get_parameters(exp, model = model)
    res <- results(run_experiment(exp,
      model = model,
      parameters = pars
    ))$associations
    # B to A should not form
    expect_true(all(res$value[res$s1 == "B" & res$s2 == "A"] == 0))
    # C to A should not form
    expect_true(all(res$value[res$s1 == "C" & res$s2 == "A"] == 0))
    # C to B should not form
    expect_true(all(res$value[res$s1 == "C" & res$s2 == "B"] == 0))
    # A to C should not form
    expect_true(all(res$value[res$s1 == "A" & res$s2 == "C"] == 0))
    # A to B should form
    expect_true(any(res$value[res$s1 == "A" & res$s2 == "B"] > 0))
    # B to C should form
    expect_true(any(res$value[res$s1 == "B" & res$s2 == "C"] > 0))
  })
}

# Test that directional models can cope with one period (simultaneous)

exp <- data.frame(group = "G", phase1 = c("10AB"))
for (model in models_to_test) {
  test_that(sprintf("model %s copes with one period", model), {
    pars <- get_parameters(exp, model = model)
    res <- results(run_experiment(exp,
      model = model,
      parameters = pars
    ))$associations
    # A to B should form
    expect_true(any(res$value[res$s1 == "A" & res$s2 == "B"] > 0))
    # B to A should form
    expect_true(any(res$value[res$s1 == "B" & res$s2 == "A"] > 0))
  })
}
