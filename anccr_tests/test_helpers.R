library(calmr)
library(testthat)
library(tidyverse)
# Define some convenience functions
put_eventlog <- function(eventlog, args) {
  e <- experience(args)[[1]]
  m <- args@arguments$mapping[[1]]
  # doesn't really matter if we cut the experience
  e <- e[seq_len(nrow(eventlog)), ]
  e$time <- eventlog[, 2]
  e$stimulus <- m$unique_functional_stimuli[eventlog[, 1]]
  e$reward_mag <- eventlog[, 3]
  experience(args) <- list(e)
  args
}

join_results <- function(md, cd, rmap) {
  # first thing is to split  up prc and src
  cd$prc <- cd$psrcs$prc
  cd$src <- cd$psrcs$src

  stopifnot(all(rmap$matlab %in% names(md)))
  stopifnot(all(rmap$calmr %in% names(cd)))

  # transform back calmr results
  twos <- sapply(c("e_ij", "e_i", "m_i", "delta"),
    function(i) t(cd[[i]]),
    simplify = FALSE
  )
  threes <- sapply(c("m_ij", "prc", "src", "nc", "anccr", "rews", "das", "qs"),
    function(i) {
      x <- aperm(cd[[i]], c(2, 3, 1))
      rownames(x) <- NULL
      x
    },
    simplify = FALSE
  )
  # collapse DAs from calmr
  cdas <- apply(threes$das, 3, sum)
  threes$das <- array(cdas, dim = c(length(cdas), 1))
  cd <- c(twos, threes)

  # put results together
  res <- list()
  for (r in seq_len(nrow(rmap))) {
    cname <- paste0(rmap[r, ], collapse = ":")
    res[[cname]] <- list(md[[rmap$matlab[r]]], cd[[rmap$calmr[r]]])
  }
  res
}

assert_joint_results <- function(jres) {
  nfields <- length(jres)
  fieldnames <- names(jres)

  print("#### Asserting dimensionality ####")
  for (f in seq_len(nfields)) {
    print(fieldnames[f])

    show_failure(expect_setequal(
      dim(jres[[f]][[1]]),
      dim(jres[[f]][[2]])
    ))
  }
  print("#### Asserting values ####")
  for (f in seq_len(nfields)) {
    print(fieldnames[f])
    show_failure(expect_equal(
      unname(c(jres[[f]][[1]])),
      unname(c(
        jres[[f]][[2]]
      ))
    ))
  }
  # return plot
  df <- bind_rows(sapply(
    names(jres),
    function(x) {
      data.frame(
        name = x,
        matlab = c(jres[[x]][[1]]),
        calmr = c(jres[[x]][[2]])
      )
    },
    simplify = FALSE
  ))

  df %>% ggplot(aes(x = matlab, y = calmr, colour = name)) +
    geom_point() +
    geom_abline(slope = 1) +
    facet_wrap(~name)
}

join_parameters <- function(md, cd, cmap) {
  stopifnot(all(cmap$matlab %in% names(md)))
  stopifnot(all(cmap$calmr %in% names(cd)))
  res <- list()
  for (r in seq_len(nrow(cmap))) {
    cname <- paste0(cmap[r, ], collapse = ":")
    res[[cname]] <- list(c(md[[cmap$matlab[r]]]), c(cd[[cmap$calmr[r]]]))
  }
  res
}
