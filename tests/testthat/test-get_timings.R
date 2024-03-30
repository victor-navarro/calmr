test_that("can return trial parameters for simple designs", {
  des <- parse_design(get_design("blocking"))
  tims <- get_timings(des)
  expect_setequal(
    names(tims$mean_ITI),
    unique(trials(des)$trial_names)
  )
})

test_that("can return transition parameters for simple designs", {
  des <- parse_design(get_design("blocking"))
  tims <- get_timings(des)
  expect_setequal(
    unique(lapply(tims$transition_delay, names)),
    unique(unlist(des@mapping$transitions))
  )
})

test_that("can return trial parameters for complicated designs", {
  des <- data.frame(g = "A", p1 = "1A>B>(US)/1(US)>AB", r1 = TRUE)
  des <- parse_design(des)
  tims <- get_timings(des)
  expect_setequal(
    names(tims$mean_ITI),
    unique(trials(des)$trial_names)
  )
})

test_that("can return transition parameters for complicated designs", {
  des <- data.frame(g = "A", p1 = "1A>B>(US)/1(US)>AB", r1 = TRUE)
  des <- parse_design(des)
  tims <- get_timings(des)
  expect_setequal(
    unname(unlist(lapply(tims$transition_delay, names))),
    unname(unlist(des@mapping$transitions))
  )
})
