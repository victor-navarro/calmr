test_that("parsing empty string", {
  expect_true(
    is.null(phase_parser(""))
  )
})

test_that("simple parsing of nominal stimuli", {
  expect_setequal(
    phase_parser("10AB(AB)>(US)")$general_info$func2nomi,
    c("A", "B", "AB", "US")
  )
})

test_that("simple parsing of functional stimuli", {
  expect_setequal(
    phase_parser("10AB(AB)>(US)")$general_info$func2nomi,
    c("A", "B", "AB", "US")
  )
})

test_that("serious parsing of nominal stimuli", {
  expect_setequal(
    phase_parser("10A(US_a)/10A>(US_b)")$general_info$func2nomi,
    c("A", "US_a", "US_b")
  )
})

test_that("serious parsing of functional stimuli", {
  expect_setequal(
    phase_parser("10A(US_a)/10A(US_b)")$general_info$nomi2func,
    c("A", "US")
  )
})

test_that("parsing of many complex stimuli into functional stimuli", {
  expect_setequal(
    phase_parser("10(A_a)(US_a)/10(A_b)(US_b)")$general_info$nomi2func,
    c("A", "US")
  )
})

test_that("mixing up simple and complex stimuli", {
  expect_setequal(
    phase_parser("10(A_a)(US_a)/10B(US_b)")$general_info$nomi2func,
    c("A", "B", "US")
  )
})

test_that("mixing up trial notation", {
  expect_setequal(
    phase_parser("10(A_a)>(US_a)/10(B)>(US_b)")$general_info$nomi2func,
    c("A", "B", "US")
  )
})

test_that("> produces expected periods", {
  expect_equal(
    length(phase_parser("10A>(B_a)")$trial_info$`10A>(B_a)`$functionals),
    2
  )
  expect_equal(
    length(phase_parser("10A>(B_a)>A")$trial_info$`10A>(B_a)>A`$functionals),
    3
  )
})

test_that("checking that # works in the syntax", {
  expect_false(phase_parser("1X")$general_info$is_test[[1]])
  expect_true(phase_parser("1X#")$general_info$is_test[[1]])
  expect_true(phase_parser("2A(US)/1X#")$general_info$is_test[[2]])
})

test_that("complex stimuli can be numbered", {
  expect_setequal(
    phase_parser("10(US1)>(US2)")$general_info$func2nomi,
    c("US1", "US2")
  )
})
