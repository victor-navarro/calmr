test_that("can return trial parameters for simple designs", {
  des <- parse_design(get_design("blocking"))
  tims <- get_timings(des, model = "ANCCR")
  expect_setequal(
    tims$trial_ts$trial,
    unique(trials(des)$trial_names)
  )
})

test_that("can return transition parameters for simple designs", {
  des <- parse_design(get_design("blocking"))
  tims <- get_timings(des, model = "TD")
  expect_setequal(
    tims$transition_ts$transition,
    unique(unlist(des@mapping$transitions))
  )
})

test_that("can return trial parameters for complicated designs", {
  des <- data.frame(g = "A", p1 = "!1A>B>(US)/1(US)>AB")
  des <- parse_design(des)
  tims <- get_timings(des, model = "TD")
  expect_setequal(
    tims$trial_ts$trial,
    unique(trials(des)$trial_names)
  )
})

test_that("can return transition parameters for complicated designs", {
  des <- data.frame(g = "A", p1 = "!1A>B>(US)/1(US)>AB")
  des <- parse_design(des)
  tims <- get_timings(des, model = "TD")
  expect_setequal(
    tims$transition_ts$transition,
    unname(unlist(des@mapping$transitions))
  )
})

test_that("returns only timings that are relevant for the model", {
  des <- data.frame(g = "A", p1 = "!1A>B>(US)/1(US)>AB")
  des <- parse_design(des)
  tims <- get_timings(des, model = "ANCCR")
  expect_true(!("trials" %in% names(tims)))
})

test_that("returns only global timings that are relevant for the model", {
  des <- data.frame(g = "A", p1 = "!1A>B>(US)/1(US)>AB")
  des <- parse_design(des)
  expect_true(!("time_resolution" %in%
    names(get_timings(des, model = "ANCCR"))))
  expect_true(("time_resolution" %in%
    names(get_timings(des, model = "TD"))))
})
