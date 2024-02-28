decay_fun <- function(t1, t2, resolution, decay) {
  # act at t1 decays with rate decay
  decay^floor((t2 - t1) / resolution)
}
decay_fun(0, .9, .1, .3)

# Implementation will be close to the Ludvig et al paper
