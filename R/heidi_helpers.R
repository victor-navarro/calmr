# Some functions to help the HeiDI model (HDI2020, HD2022)
# Calculation of combined v
.combV <- function(v, pre_func, post_func) {
  # v is a weight matrix,
  # pre_func is a character vector of the stimuli being presented
  # post_func is a character vector of the stimuli being predicted
  #
  # returns a matrix of dimensions pre_func x post_func, with the combV values
  mat <- array(0,
    dim = c(1, length(post_func)),
    dimnames = list(paste0(pre_func, collapse = ","), post_func)
  )
  for (po in post_func[!(post_func %in% pre_func)]) {
    mat[1, po] <- sum(v[pre_func, po]) +
      (sum(v[pre_func, po]) * (sum(v[po, pre_func])))
  }
  return(mat)
}

# Calculation of chain v
.chainV <- function(v, pre_func, post_func) {
  # v is a weight matrix,
  # pre_func is a character vector of the stimuli being presented
  # post_func is a character vector of the stimuli being predicted
  #
  # The trick here is to obtain the chainV from every
  # pre_func stimulus to every post_func stimulus,
  # while hitting every absent stimulus along the way
  #
  # Returns a matrix of dimensions pre_func post_func with chainV values

  # Preallocate zeros
  mat <- array(0, dim = c(
    length(pre_func),
    length(post_func)
  ), dimnames = list(pre_func, post_func))

  # find the absent stimuli
  allstims <- rownames(v)
  absent <- setdiff(allstims, pre_func)

  if (length(absent)) {
    # a for loop for readability
    for (po in absent) {
      for (pr in pre_func) {
        total_sum <- 0
        for (a in absent) {
          total_sum <- total_sum + v[pr, a] * .combV(v, a, po)[, po]
        }
        mat[pr, po] <- total_sum
      }
    }
  }
  return(mat)
}

# Calculation of chain v with Similarity
.chainVSim <- function(
    v, as_nomi, as_avg,
    pre_nomi, pre_func, post_func) {
  # Same as above, but with similarity of retrieved
  # and nominal alphas modulating the the chain
  # as: a vector of nominal saliencies

  # Preallocate zeros
  mat <- array(0, dim = c(
    length(pre_func),
    length(post_func)
  ), dimnames = list(pre_func, post_func))

  # get absent stimuli
  allstims <- rownames(v)
  absent <- setdiff(allstims, pre_func)

  # a for loop for readability
  if (length(absent)) {
    # get retrieved alphas
    retrieved_as <- .absentalphas(v = v, pre_func = pre_func)
    # get the average of their nominal alphas (TEMPORARY)
    nomi_avg_as <- as_avg[absent]
    for (po in absent) {
      for (pr in pre_func) {
        total_sum <- 0
        for (a in absent) {
          total_sum <- total_sum +
            .alphaSim(retrieved_as[a], nomi_avg_as[a]) *
              v[pr, a] * .combV(v, a, po)[, po]
        }
        mat[pr, po] <- total_sum
      }
    }
  }
  return(mat)
}

# Distribution of R among stimuli
.distR <- function(alphas, combv, chainv) {
  # Distributes the associative strength among all stimuli (alphas)
  # returns a matrix of dimensions length(alphas) x ncols(combv)
  mat <- (alphas / sum(alphas)) %*% (combv + colSums(chainv))
  rownames(mat) <- names(alphas)
  return(mat)
}

.getalphas <- function(alphas_nomi, v, pre_nomi, pre_func, fsnames, nomi2func,
                       absent_func = .absentalphas) {
  # gets the saliencies for a given trial
  # it performs two actions:
  # 1. populates a vector of saliencies for functional stimuli
  # [this based on the saliency (alphas) of the
  # nominal stimuli on the trial (pre_nomi)]
  # 2. calculates the saliency for absent
  # stimuli, via the .absentalphas function
  as <- stats::setNames(rep(0, length(fsnames)), fsnames)
  # Annoying bit again, see main function
  as[nomi2func[pre_nomi]] <- alphas_nomi[pre_nomi]
  # now do absent stimuli
  absent <- names(as[as == 0])
  if (length(absent)) {
    as[absent] <- absent_func(v = v, pre_func = pre_func)
  }
  as
}

# Function to calculate the alpha of absent stimuli (SIMPLE)
.absentalphas <- function(v, pre_func) {
  # NOTE (VN): This implementation simplifies the
  # absent alpha to be only the sum of forward associations
  # v is a weight matrix,
  # pre_func is a character vector of the stimuli being presented
  #
  # Returns a vector of alphas equal to the number of absent of stimuli
  allstims <- rownames(v)
  absent <- setdiff(allstims, pre_func)
  as <- stats::setNames(rep(0, length(absent)), absent)
  for (ab in absent) {
    as[ab] <- sum(abs(v[pre_func, ab]))
  }
  as
}

# Function to calculate the alpha of absent stimuli (COMPLEX)
.absentalphas_complex <- function(v, pre_func) {
  # NOTE (VN): This implementation goes through chained associations
  # v is a weight matrix,
  # pre_func is a character vector of the stimuli being presented
  #
  # Returns a vector of alphas equal to the number of absent of stimuli
  allstims <- rownames(v)
  absent <- setdiff(allstims, pre_func)
  as <- stats::setNames(rep(0, length(absent)), absent)
  for (ab in absent) {
    total_sum <- 0
    for (pr in pre_func) {
      total_sum <- total_sum + v[pr, ab] # the direct association
      # now do the indirect associations via other absent stimuli
      int <- setdiff(setdiff(absent, ab), pr) # the other absent stimuli
      if (length(int)) {
        total_sum <- total_sum + sum(
          sapply(int, function(i) v[pr, i] * v[i, ab], USE.NAMES = FALSE)
        )
      }
    }
    as[ab] <- abs(total_sum) # Note the absolute function; important to study it
  }
  as
}

# Returns the similarity between two (salience) values
.alphaSim <- function(i, j) {
  (i / (i + abs(i - j))) * (j / (j + abs(i - j)))
}
