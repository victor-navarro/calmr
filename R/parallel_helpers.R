.parallel_standby <- function(pb) {
  if (!methods::is(future::plan(), "sequential")) {
    pb(amount = 0, message = "Setting parallel backend ...")
  }
}
