.parallel_standby <- function(pb) {
  if (!is(future::plan(), "sequential")) {
    pb(amount = 0, message = "Setting parallel backend ...")
  }
}
