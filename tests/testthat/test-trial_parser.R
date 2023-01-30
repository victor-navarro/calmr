test_that("trial_parser works", {
  #### trial_parser tests ####
  #parsing of an empty string
  expect_setequal(
    trial_parser("")$trial_repeats, 0)
  #simple parsing of nominal stimuli
  expect_setequal(
    trial_parser("10AB(AB)>(US)")$unique_nominal_stimuli, c("A", "B", "AB", "US"))
  #same, but with functional stimuli
  expect_setequal(
    trial_parser("10AB(AB)>(US)")$unique_functional_stimuli, c("A", "B", "AB", "US"))
  #serious parsing of nominal stimuli
  expect_setequal(
    trial_parser("10A(US_a)/10A>(US_b)")$unique_nominal_stimuli, c("A", "US_a", "US_b"))
  #serious parsing of functional stimuli
  expect_setequal(
    trial_parser("10A(US_a)/10A(US_b)")$unique_functional_stimuli, c("A", "US"))
  #parsing of many complex stimuli into functional stimuli
  expect_setequal(
    trial_parser("10(A_a)(US_a)/10(A_b)(US_b)")$unique_functional_stimuli, c("A", "US"))
  #mixing up simple and complex stimuli
  expect_setequal(
    trial_parser("10(A_a)(US_a)/10B(US_b)")$unique_functional_stimuli, c("A", "B", "US"))
  #mixing up trial notation
  expect_setequal(
    trial_parser("10(A_a)>(US_a)/10(B)>(US_b)")$unique_functional_stimuli, c("A", "B", "US"))
  #testing that > works in the syntax
  expect_equal(
    trial_parser("10A>(B_a)")$trial_post_functional[[1]], "B")
  #same but with functional stimuli
  expect_equal(
    trial_parser("10(B_x)>(B_y)")$trial_pre_functional[[1]], "B")
  #checking that # works in the syntax
  expect_false(trial_parser("1X")$is_test[[1]])
  expect_true(trial_parser("1X#")$is_test[[1]])
  #another check of the above
  expect_true(trial_parser("2A(US)/1X#")$is_test[[2]])
})
