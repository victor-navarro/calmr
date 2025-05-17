# TODO: All absent alphas functions should be made modular in the future
test_that(".absentalphas_complex works", {
  v <- gen_ss_weights(letters[1:3])
  v["a", "b"] <- pi
  expect_equal(sum(unname(.absentalphas_complex(v, letters[1]))), pi)
})
