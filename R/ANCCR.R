ANCCR <- function(
    parameters, experience,
    mapping, debug = FALSE, ...) {
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
  sampling_times <- experience[, "time"]

  #### Model initialization ####

  # TODO: Do the omission logs
  # not even implemented in the original code
  omidx <- array(-1, dim = c(nt, 2))
  omtrue <- array(FALSE, dim = c(nt, 2))

  # TODO: Do the opto logs
  optolog <- array(FALSE, dim = c(nt, 2))

  e_ij <- e_i <- m_i <- delta <- array(0,
    dim = c(length(fsnames), nt),
    dimnames = list(fsnames, seq_len(nt))
  )

  m_ij <- prc <- src <- nc <- anc <- rs <- array(0, dim = c(
    length(fsnames), length(fsnames), nt
  ), dimnames = list(fsnames, fsnames, seq_len(nt)))

  r <- array(0, dim = c(
    length(fsnames),
    length(fsnames)
  ), dimnames = list(fsnames, fsnames))

  numevents <- array(0,
    dim = c(length(fsnames), 1),
    dimnames = list(fsnames, NULL)
  )
  da <- array(0, dim = c(nt, 1))
  imct <- parameters$betas > parameters$thresholds
  # calculate t_constant based on the t_ratio
  tcons <- parameters$t_ratio * with(
    parameters,
    sum(unlist(lapply(list(mean_ITI, post_trial_delay), mean)))
  )
  t_constants <- rep(tcons, nt)
  nextt <- 1
  numsampling <- 0

  for (timestep in seq_len(nt)) {
    if (debug) print(timestep)

    skip <- FALSE
    event <- experience[timestep, "stimulus"] # event
    absents <- fsnames[!(fsnames == event)]

    # deal with omission
    if (event %in% omidx[, 1]) {
      if (!omtrue[, 1] == event) {
        delta[, timestep] <- delta[, timestep - 1]
        e_ij[, timestep] <- e_ij[, timestep - 1]
        m_ij[, timestep] <- m_ij[, timestep - 1]
        prc[, timestep] <- prc[, timestep - 1]
        src[, timestep] <- src[, timestep - 1]
        nc[, timestep] <- nc[, timestep - 1]
        skip <- TRUE
      }
    }


    if (!skip) {
      # This is the serious block
      numevents[event, ] <- numevents[event, ] + 1
      if (parameters$use_exact_mean) {
        alphat <- exp(
          -parameters$alpha_exponent * timestep *
            (parameters$alpha_init - parameters$alpha_min) +
            parameters$alpha_min
        )
      } else {
        alphat <- 1 / numevents[event]
      }

      if (timestep > 1) {
        # Update delta
        delta[, timestep] <- delta[, timestep - 1] *
          parameters$gammas[timestep]^(
            experience[timestep, 2] -
              experience[timestep - 1, 2]
          )
        # Update eligibility trace
        e_ij[, timestep] <- e_ij[, timestep - 1] *
          parameters$gammas[timestep]^(
            experience[timestep, 2] -
              experience[timestep - 1, 2]
          )
        # Update average eligibility trace
        m_ij[, , timestep] <- m_ij[, , timestep - 1]
        anc[absents, , timestep] <-
          anc[absents, , timestep - 1]
      }
      # Delta reset
      delta[event, timestep] <- 1
      # Increment elegibility trace for the event that occurred
      e_ij[event, timestep] <- e_ij[event, timestep] + 1
      # Update average eligibility trace
      m_ij[, event, timestep] <- m_ij[, event, timestep] + alphat *
        (e_ij[, timestep] - m_ij[, event, timestep]) * imct[event]
      # Calculate predecessor representations
      # Sweep subtracts
      prc[, , timestep] <- sweep(m_ij[, , timestep], 2, m_i[, timestep])
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
      nc[, , timestep] <- parameters$w * src[, , timestep] +
        (1 - parameters$w) * prc[, , timestep]

      # Indicator for causal links between events
      # VN: There is a bug in the original ANCCR code:
      # I think the expression: max([1,jt-nevent_for_edge]:jt) is meant to be
      # max([1,jt-nevent_for_edge]):jt
      edge_ts <- max(c(1, timestep - nevent_for_edge)):timestep
      i_edge <- rowMeans(nc[, event, edge_ts, drop = FALSE]) >
        parameters$threshold
      i_edge[event] <- FALSE

      browser()


      # Something about the omission state of that reward state;
      # Some omission garbage code
      if (event %in% omidx[, 2] && sum(i_edge) > 0) {
        omtrue[omidx[, 2] == event] <- omtrue[omidx[, 2] == event] | TRUE
      }

      # Calculate ANNCR for every event
      # First, Rjj
      r[event, event] <- experience[timestep, 3] # Magnitude of the US
      for (ke in 1:length(fsnames)) {
        # Update edge indicator
        # The mean in MATLAB is calculated on the 3 dimension; check
        browser()
        i_edge_ke <- mean(nc[, ke, max(max(1, timestep - nevent_for_edge):timestep)]) >
          parameters$threshold
        i_edge_ke <- FALSE
        # Update ANCCR
        anc[ke, , timestep] <- nc[ke, , timestep] * r[ke, ] -
          sum(sweep(anc[, , timestep] * delta[, timestep], i_edge_ke, FUN = "*"))
      }

      # Calculate DA response (replace if optolog says so)
      if (!optolog[timestep, 1]) {
        da[timestep] <- sum(anc[event, , timestep] * t(imct)) + beta[event]
      } else {
        da[timestep] <- optolog[timestep, 2]
      }

      # Do some extra calculations for omission
      if (event %in% omidx[, 1]) {
        je_om <- which(event == omidx[, 1])
        r[event, omidx[je_om, 2]] <- r[omidx[je_om, 2], omidx[je_om, 2]]
        imct[event] <- TRUE
      }

      imct[event] <- imct[event] | da[timestep] + beta[event] > parameters$threshold

      # Update estimated reward value
      rs[, , timestep] <- r
      if (da[timestep] >= 0) {
        # Positive (or zero) DA response update rule
        r[, event] <- r[, event] * parameters$alpha_r * (experience[timestep, 3] - r[, event])
      } else {
        # Negative DA response update rule (overprediction)
        if (any(i_edge)) {
          r[i_edge, event] <- r[i_edge, event] -
            parameters$alpha_r * r[i_edge, event] *
              (delta[i_edge, timestep] / numevents[i_edge]) /
              sum((delta[i_edge] / numevents[i_edge]))
        } else {
          r[, event] <- r[, event] # ???
        }
      }
    }

    # Update sample eligibility trace
    if (timestep < nt) {
      # Time to sample baseline b/t events
      subsamplingtime <- sampling_times[
        sampling_times >= experience[timestep, 2] & sampling_times < experience[timestep + 1, 2]
      ]
      e_i[, timestep + 1] <- e_i[, timestep] * parameters$gammas[timestep]^parameters$sampling_interval
      if (length(subsamplingtime)) {
        for (jjt in nextt:timestep) {
          e_i[experience[jjt, 1], timestep + 1] <- e_i[experience[jjt], timestep + 1] +
            parameters$gammas[timestep]^(subsamplingtime[1] - experience[jjt, 2])
        }
        nextt <- timestep + 1
      }
      # Update alpha of sample elegibility trace
      if (parameters$use_exact_mean) {
        if (is.array(parameters$alphas)) {
          alphat <- parameters$alphas
        } else {
          alphat <- exp(
            -parameters$alpha_pars$exponent * (timestep - 0) *
              (parameters$alpha_pars$init - parameters$alpha_pars$min) +
              parameters$alpha_pars$min
          )
        }
      } else {
        alphat <- 1 / (numsampling + 1)
      }
      # Update average sample eligibility trace
      m_i[, timestep + 1] <- m_i[, timestep] + parameters$k + alphat *
        (e_i[, timestep + 1] - m_i[, timestep])
      for (iit in 2:length(subsamplingtime)) {
        if (parameters$use_exact_mean) {
          if (is.array(parameters$alphas)) {
            alphat <- parameters$alphas
          } else {
            alphat <- exp(
              -parameters$alpha_pars$exponent * (timestep - 0) *
                (parameters$alpha_pars$init - parameters$alpha_pars$min) +
                parameters$alpha_pars$min
            )
          }
        } else {
          alphat <- 1 / (numsampling + iit)
        }
        e_i[, timestep + 1] <- e_i[, timestep + 1] * parameters$gammas[timestep]^parameters$sampling_interval
        m_i[, timestep + 1] <- m_i[, timestep + 1] +
          parameters$k * alphat * (e_i[, timestep + 1] - m_i[, timestep + 1])
      }
      numsampling <- numsampling + length(subsamplingtime)
    }
  }
  1
}
