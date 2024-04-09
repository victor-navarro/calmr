#' Train the ANCCR model
#'
#' @param parameters A list containing the model parameters,
#' as returned by `get_parameters()`.
#' @param timings A list containing the design timings,
#' as returned by `get_timings()`.
#' @param experience A data.frame specifying trials as rows,
#' as returned by `make_experiment`
#' @param mapping A named list specifying trial and stimulus mapping,
#' as returned by `make_experiment`
#' @param debug Logical specifying whether to print debug information.
#' @param debug_t Whether to invoke a `browser` at
#' the end of a timestep equal to debug_t.
#' @param ... Additional named arguments
#' @return A list with raw results
#' @note This model is in a highly experimental state. Use with caution.
#' @noRd
ANCCR <- function(
    parameters, timings, experience,
    mapping, debug = FALSE, debug_t = -1, ...) {
  # TODO: Deal with omission as you would do with probe trials

  #### Loose parameters ####
  nevent_for_edge <- 0
  # VN: There are some expressions that subtract this value from
  # stimulus pointers (e.g., l 134). They are wrapped with square brackets and
  # used for integer arrays alongside a 1. If the subtraction yields
  # a negative number, the array will start from 1 instead.
  # I don't see a use for this at the moment but it is implemented

  # Initialization
  nt <- nrow(experience)
  fsnames <- mapping$unique_functional_stimuli

  #### Model initialization ####

  # TODO: Do the omission logs
  # not even implemented in the original code
  # omidx <- array(-1, dim = c(nt, 2))
  # omtrue <- array(FALSE, dim = c(nt, 2))

  # TODO: Do the opto logs
  optolog <- array(FALSE, dim = c(nt, 2))

  e_ij <- e_i <- m_i <- delta <- imcts <- array(0,
    dim = c(length(fsnames), nt),
    dimnames = list(fsnames, NULL)
  )

  m_ij <- prc <- src <- ncs <- anccrs <- cws <- das <- array(0, dim = c(
    length(fsnames), length(fsnames), nt
  ), dimnames = list(fsnames, fsnames, NULL))

  r <- array(0, dim = c(
    length(fsnames),
    length(fsnames)
  ), dimnames = list(fsnames, fsnames))
  # prepopulate values based on their innateness
  diag(r) <- parameters$reward_magnitude

  numevents <- array(0,
    dim = c(length(fsnames), 1),
    dimnames = list(fsnames, NULL)
  )
  imct <- parameters$betas > parameters$threshold
  # calculate t_constant based on the t_ratio
  if (is.na(parameters$t_constant)) {
    parameters$t_constant <- parameters$t_ratio *
      with(
        timings$trial_ts,
        sum(unlist(lapply(list(mean_ITI), mean)))
      )
  }
  t_constants <- rep(parameters$t_constant, nt)
  gammas <- exp(-1 / t_constants)
  nextt <- 1
  numsampling <- 0

  for (timestep in seq_len(nt)) {
    if (debug) print(timestep) # nocov
    skip <- FALSE
    event <- experience[timestep, "stimulus"] # event
    absents <- fsnames[!(fsnames == event)]

    # TODO: deal with omission
    # if (event %in% omidx[, 1]) {
    #   if (!omtrue[, 1] == event) {
    #     delta[, timestep] <- delta[, timestep - 1]
    #     e_ij[, timestep] <- e_ij[, timestep - 1]
    #     m_ij[, timestep] <- m_ij[, timestep - 1]
    #     prc[, timestep] <- prc[, timestep - 1]
    #     src[, timestep] <- src[, timestep - 1]
    #     ncs[, timestep] <- ncs[, timestep - 1]
    #     skip <- TRUE
    #   }
    # }

    if (!skip) {
      # save imcts
      imcts[, timestep] <- imct
      # This is the serious block
      numevents[event, ] <- numevents[event, ] + 1
      alphat <- .anccr_get_alpha(
        denom = numevents[event, ],
        parameters = parameters,
        timestep = timestep
      )

      if (timestep > 1) {
        # Update delta
        delta[, timestep] <- delta[, timestep - 1] *
          gammas[timestep]^(
            experience[timestep, "time"] -
              experience[timestep - 1, "time"]
          )
        # Update eligibility trace
        e_ij[, timestep] <- e_ij[, timestep - 1] *
          gammas[timestep]^(
            experience[timestep, "time"] -
              experience[timestep - 1, "time"]
          )
        # Set eligibility trace and anccrs
        m_ij[, , timestep] <- m_ij[, , timestep - 1]
        anccrs[absents, , timestep] <- anccrs[absents, , timestep - 1]
      }
      # Delta reset
      delta[event, timestep] <- 1
      # Increment eligibility trace for the event that occurred by + 1
      e_ij[event, timestep] <- e_ij[event, timestep] + 1
      # Update predecessor representation
      m_ij[, event, timestep] <- m_ij[, event, timestep] + alphat *
        (e_ij[, timestep] - m_ij[, event, timestep]) * imct[event]
      # Calculate predecessor representation contingency
      # Sweep subtracts
      prc[, , timestep] <- sweep(m_ij[, , timestep], 1, m_i[, timestep])
      # Calculate successor representation
      src[, , timestep] <- sweep(
        prc[, , timestep], 2,
        m_i[, timestep],
        FUN = "*"
      ) / m_i[, timestep]
      # Zero out values that may approach -Inf
      belowminrate <- (m_i[, timestep] / t_constants[timestep]) <
        parameters$minimum_rate
      src[belowminrate, , timestep] <- 0

      # Something to calculate contingency only after experiencing first outcome
      prc[numevents == 0, , timestep] <- prc[, numevents == 0, timestep] <-
        src[numevents == 0, , timestep] <- src[, numevents == 0, timestep] <-
        r[numevents == 0, ] <- r[, numevents == 0] <- 0

      # Calculate net contingency
      ncs[, , timestep] <- parameters$w * src[, , timestep] +
        (1 - parameters$w) * prc[, , timestep]

      # Indicator for causal links between events
      # VN: There is a bug in the original ANCCR code:
      # The expression: max([1,jt-nevent_for_edge]:jt) is meant to be
      # max([1,jt-nevent_for_edge]):jt
      # This is because the nevent_for_edge is meant
      # to allow averaging across timesteps but the bugged
      # line only returns a scalar
      edge_ts <- max(c(1, timestep - nevent_for_edge)):timestep
      i_edge <- rowMeans(ncs[, event, edge_ts, drop = FALSE]) >
        parameters$threshold
      i_edge[event] <- FALSE

      # # TODO: Something about the omission state of that reward state;
      # # Some omission garbage code
      # if (event %in% omidx[, 2] && sum(i_edge) > 0) {
      #   omtrue[omidx[, 2] == event] <- omtrue[omidx[, 2] == event] | TRUE
      # }

      # Calculate ANNCR for every event
      # First, set reward magnitude of the event (r_jj)
      r[event, event] <- experience[timestep, "reward_mag"]
      for (ke in fsnames) {
        # Update edge indicator
        i_edge_ke <- rowMeans(ncs[, ke, edge_ts, drop = FALSE]) >
          parameters$threshold
        i_edge_ke[ke] <- FALSE
        # Update ANCCR
        anccrs[ke, , timestep] <- ncs[ke, , timestep] * r[ke, ] -
          colSums(sweep(anccrs[, , timestep] * delta[, timestep], 1,
            i_edge_ke,
            FUN = "*"
          ))
      }

      # Calculate DA response (replace if optolog says so)
      if (!optolog[timestep, 1]) {
        # conditioned DA
        das[event, , timestep] <- anccrs[event, , timestep] *
          as.numeric(imct)
        # unconditioned DA
        das[event, event, timestep] <- das[event, event, timestep] +
          parameters$betas[event] * parameters$add_beta
      } else {
        # TODO: Optolog related stuff
        # das[event, , timestep] <- optolog[timestep, 2]
      }
      # # TODO: Do some extra calculations for omission
      # if (event %in% omidx[, 1]) {
      #   je_om <- which(event == omidx[, 1])
      #   r[event, omidx[je_om, 2]] <- r[omidx[je_om, 2], omidx[je_om, 2]]
      #   imct[event] <- TRUE
      # }
      # Total dopamine
      tda <- sum(das[event, , timestep])
      # Update meaningful causal target index
      imct[event] <- imct[event] |
        (
          (tda + parameters$betas[event]) >
            parameters$threshold
        )

      # Update estimated reward value
      cws[, , timestep] <- r
      # Learning
      if (tda >= 0) {
        # Positive (or zero) DA response update rule
        r[, event] <- r[, event] +
          parameters$alpha_reward *
            (experience[timestep, "reward_mag"] - r[, event])
      } else {
        # Negative DA response update rule (overprediction)
        if (any(i_edge)) { # nocov start
          r[i_edge, event] <- r[i_edge, event] -
            parameters$alpha_reward * r[i_edge, event] *
              ((delta[i_edge, timestep] / numevents[i_edge, ]) /
                sum((delta[i_edge, timestep] / numevents[i_edge, ]))) # nocov end
        } else {
          r[, event] <- r[, event] # ??? Must be vestigial
        }
      }
    }
    # Update sample eligibility trace
    if (timestep < nt) {
      # Time to sample baseline b/t events
      # VN: The function below is about 100 times faster than the original
      subsamplingtime <- .seq_gen(
        experience[timestep, "time"],
        experience[timestep + 1, "time"],
        parameters$sampling_interval
      )

      e_i[, timestep + 1] <- e_i[, timestep] *
        gammas[timestep]^parameters$sampling_interval
      if (length(subsamplingtime)) {
        for (jjt in nextt:timestep) {
          # # TODO: omission log stuff
          # if (experience[jjt, "stimulus"] %in% omidx[, 1]) {
          #   if (!omtrue[omidx[, 1] == experience[jjt, "stimulus"]]) {
          #     1 #
          #   }
          # }
          e_i[experience[jjt, "stimulus"], timestep + 1] <-
            e_i[experience[jjt, "stimulus"], timestep + 1] +
            gammas[timestep]^(subsamplingtime[1] - experience[jjt, "time"])
        }
        nextt <- timestep + 1
      }
      # Update alpha of sample eligibility trace
      alphat <- .anccr_get_alpha(
        denom = numsampling + 1,
        parameters = parameters,
        timestep = timestep
      )

      # Update average sample eligibility trace
      # Name: Baseline predecessor representation
      m_i[, timestep + 1] <- m_i[, timestep] + parameters$k * alphat *
        (e_i[, timestep + 1] - m_i[, timestep])
      for (iit in seq_len(length(subsamplingtime))[-1]) {
        alphat <- .anccr_get_alpha(
          denom = numsampling + iit,
          parameters = parameters,
          timestep = timestep
        )
        e_i[, timestep + 1] <- e_i[, timestep + 1] *
          gammas[timestep]^parameters$sampling_interval
        m_i[, timestep + 1] <- m_i[, timestep + 1] +
          parameters$k * alphat * (e_i[, timestep + 1] - m_i[, timestep + 1])
      }
      numsampling <- numsampling + length(subsamplingtime)
    }
    if (timestep == debug_t) browser() # nocov
  }

  # calculate q values
  qs <- src * cws
  # calculate probabilities (funny softmax)
  cqs <- (qs + parameters$cost) * parameters$temperature
  ps <- exp(cqs) / (exp(0) + exp(cqs)) # nolint: object_usage_linter.

  # some reshaping before return
  twos <- sapply(c("e_ij", "e_i", "m_i"),
    function(i) t(get(i)),
    simplify = FALSE
  )
  threes <- sapply(
    c(
      "m_ij", "prc", "src", "ncs", "anccrs",
      "cws", "das", "qs", "ps"
    ),
    function(i) {
      x <- aperm(get(i), c(3, 1, 2))
      rownames(x) <- NULL
      x
    },
    simplify = FALSE
  )
  # bundle prc and src
  psrcs <- list(PRC = threes$prc, SRC = threes$src)
  threes <- threes[c("m_ij", "ncs", "anccrs", "cws", "das", "qs", "ps")]

  names(twos) <- c(
    "ij_eligibilities", "i_eligibilities",
    "i_base_rate"
  )
  names(threes) <- c(
    "ij_base_rate", "net_contingencies",
    "anccrs", "causal_weights", "dopamines", "action_values",
    "probabilities"
  )

  c(twos, threes, list(representation_contingencies = psrcs))
}
