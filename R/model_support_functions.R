# An assortment of functions to support models

# Generate a matrix with named columns and rows
gen_ss_weights <- function(stims, default_val = 0) {
  array(default_val, dim = rep(length(stims), 2), dimnames = list(stims, stims))
}

# Generate a three dimensional matrix with named axes
gen_os_values <- function(stims, default_val = -1) {
  array(default_val,
    dim = rep(length(stims), 3),
    dimnames = list(stims, stims, stims)
  )
}

# Makes a onehot representation of the stimulus vector, given all stimuli
.makeOH <- function(s, stimnames) {
  return(as.numeric(stimnames %in% s))
}


# Carries out a comparison process in a recursive manner
.comparator_proc <- function(act, i, j, K, o, gammas, order, debug = FALSE) {
  ks <- setdiff(K, c(i, j))
  if (!length(ks)) {
    return(act[i, j])
  } # early exit if there are no comparators available
  if (order) { # order > 0
    val <- act[i, j] -
      sum(gammas[ks] * o[i, ks, j] *
        # recursion from i to k (link 2)
        sapply(ks, function(x) {
          .comparator_proc(
            act = act,
            i = i,
            j = x,
            K = K,
            o = o,
            gammas = gammas,
            order = order - 1,
            debug = debug
          )
        }) *
        # recursion from k to j (link 3)
        sapply(ks, function(x) {
          .comparator_proc(
            act = act,
            i = x,
            j = j,
            K = K,
            o = o,
            gammas = gammas,
            order = order - 1,
            debug = debug
          )
        }))
  } else {
    # order 0; recursion stops here
    val <- act[i, j] -
      sum(gammas[ks] * o[i, ks, j] * act[i, ks] * act[ks, j])
  }
  if (debug) cat("Order:", order, "\n", "To", j, "via", i, "against", ks, "\n")
  if (debug) cat("Link value:", val, "\n")

  val
}

# Carries out a comparison process in a recursive manner,
# but dropping previous i from link 3
.witnauer_comparator_proc <- function(
    act, i, j, K, o,
    gammas, order, debug = FALSE) {
  ks <- setdiff(K, c(i, j))
  if (!length(ks)) {
    return(act[i, j])
  } # early exit if there are no comparators available
  if (order) { # order > 0
    val <- act[i, j] -
      sum(gammas[ks] * o[i, ks, j] *
        # recursion from i to k (link 2)
        sapply(ks, function(x) {
          .comparator_proc(
            act = act,
            i = i,
            j = x,
            K = K,
            o = o,
            gammas = gammas,
            order = order - 1,
            debug = debug
          )
        }) *
        # recursion from k to j (link 3)
        sapply(ks, function(x) {
          .comparator_proc(
            act = act,
            i = x,
            j = j,
            K = setdiff(K, i),
            o = o,
            gammas = gammas,
            order = order - 1,
            debug = debug
          )
        }))
  } else {
    # order 0; recursion stops here
    val <- act[i, j] -
      sum(gammas[ks] * o[i, ks, j] * act[i, ks] * act[ks, j])
  }
  if (debug) cat("Order:", order, "\n", "To", j, "via", i, "against", ks, "\n")
  if (debug) cat("Link value:", val, "\n")

  val
}


#### Unused ####
# Softmax function
.soft <- function(acts, temp = 1) {
  acts <- acts - max(acts)
  return(exp(acts * temp) / sum(exp(acts * temp)))
}
