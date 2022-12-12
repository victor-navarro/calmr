test_that("make_model_args works", {
  #### Testing miniblocks WITH randomization ####
  df = data.frame(Group = c("X", "Y"),
                  P1 = c("10A/#10A/10B", "2AB/6A/6B/2#AB"),
                  R1 = c(TRUE, TRUE))
  pars = get_params(df, model = "HD2022")
  #throw error if design is not parsed
  expect_error(make_model_args(df, pars))
  parsed_df = parse_design(df)
  minib_args = make_model_args(parsed_df, pars, model = "HD2022", opts = get_model_opts(miniblocks = T))
  #should generate 10 blocks for group X, each containing one of each trial type
  expect_true(all(sapply(1:3, function(x) sum(ceiling(which(minib_args$experience[[1]]$tp == x)/3)) == sum(1:10))))
  #minib_args$trial_names
  #should generate 2 blocks for group Y, each containing 1 of each AB and #AB trials, and 3 each of A and B trials
  expect_true(all(sapply(4:5, function(x) sum(ceiling(which(minib_args$experience[[2]]$tp == x)/8)))== sum(1:2)))
  expect_true(all(sapply(c(1, 3), function(x) sum(ceiling(which(minib_args$experience[[2]]$tp == x)/8)))== sum(rep(1:2, 3))))

  #### Testing miniblocks WITHOUT randomization ####
  df = data.frame(Group = c("X", "Y"),
                  P1 = c("10A/#10A/10B", "2AB/6A/6B/2#AB"),
                  R1 = c(FALSE, FALSE))
  parsed_df = parse_design(df)
  minib_args = make_model_args(parsed_df, pars, model = "HD2022", opts = get_model_opts(miniblocks = T))
  #should generate 10 blocks for group X, in repeating order (1, 2, 3, 1, 2, 3, etc.)
  expect_equal(minib_args$experience[[1]]$tp, rep(1:3, 10))
  #should generate 2 blocks for group Y, in repeating order (4, 1, 1, 1, 3, 3, 5, and so on)
  expect_equal(minib_args$experience[[2]]$tp, rep(c(4, 1, 1, 1, 3, 3, 3, 5), 2))

})
