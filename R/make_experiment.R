#' Make CalmrExperiment
#'
#' @description Makes a `CalmrExperiment` object containing
#' the arguments necessary to run an experiment.
#' @param design A design `data.frame`.
#' @param model A string specifying the model name. One of [supported_models()].
#' @param parameters Optional. Parameters for a  model as
#' returned by [get_parameters()].
#' @param timings Optional. Timings for a time-based design as
#' returned by [get_timings()]
#' @param iterations An integer specifying the number of iterations per group.
#' Default = 1.
#' @param miniblocks Whether to organize trials in miniblocks. Default = TRUE.
#' @param seed A valid seed for the RNG to make the experiment.
#' Default = NULL, in which case the current RNG is used.
#' @param .callback_fn A function for keeping track of progress. Internal use.
#' @param ... Extra parameters passed to other functions.
#' @return A [CalmrExperiment-class] object.
#' @seealso [parse_design()],
#' @note The miniblocks option will direct the sampling function to create
#' equally-sized miniblocks with random trials within a phase.
#' For example, the phase string "2A/2B" will create two miniblocks
#' with one of each trial. The phase string "2A/4B" will create two miniblocks
#' with one A trial, and 2 B trials. However, the phase string "2A/1B" will
#' not result in miniblocks, even if miniblocks here is set to TRUE.
#' @examples
#' des <- data.frame(Group = "G1", P1 = "10A>(US)")
#' ps <- get_parameters(des, model = "HD2022")
#' make_experiment(
#'   design = des, parameters = ps,
#'   model = "HD2022", iterations = 2
#' )
#' @export

make_experiment <- function(
    design,
    model,
    parameters = NULL,
    timings = NULL,
    iterations = 1,
    miniblocks = TRUE,
    seed = NULL,
    .callback_fn = NULL,
    ...) {
  # assert design is parsed
  design <- .assert_parsed_design(design)
  # get group names
  group_names <- design@raw_design[, 1]
  # assert user passed only one model
  .assert_single_model(model)
  # assert model
  model <- .assert_model(model)
  # assert model parameters
  parameters <- .assert_parameters(parameters,
    model = model, design = design
  )
  # assert timing parameters
  if (model %in% supported_timed_models()) {
    timings <- .assert_timings(timings,
      design = design, model
    )
  }

  # build the experiences for the experiment
  pb <- progressr::progressor(iterations)
  .parallel_standby(pb) # print parallel backend message
  pb(amount = 0, message = "Building experiment")
  allexps <- .with_seed(seed, {
    future.apply::future_sapply(seq_len(iterations), function(x) {
      if (!is.null(.callback_fn)) .callback_fn() # nocov
      exper <- .build_experiment(
        design = design,
        model = model,
        iterations = iterations,
        miniblocks = miniblocks,
        ...
      )
      # augment experience if necessary
      exper <- .augment_experience(exper,
        model = model, design = design,
        parameters = parameters, timings = timings, ...
      )
      pb("Building experiment")
      exper
    }, simplify = FALSE, future.seed = TRUE)
  })

  # unnest once
  allexps <- unlist(allexps, recursive = FALSE)
  # hack timings
  timings <- if (is.null(timings)) list() else timings

  # return experiment
  methods::new("CalmrExperiment",
    design = design,
    model = model,
    groups = group_names,
    parameters = stats::setNames(rep(
      list(parameters),
      length(group_names)
    ), group_names),
    timings = timings,
    experiences = allexps,
    results = methods::new("CalmrExperimentResult"),
    .model = rep(model, length(allexps)),
    .group = rep(group_names, iterations),
    .iter = rep(seq_len(iterations), each = length(group_names)),
    .seed = seed
  )
}

# general function to build experiment
.build_experiment <- function(
    design, model, iterations, miniblocks,
    .callback_fn = NULL, ...) {
  # sample trials
  des <- design@design
  samples <- lapply(des, function(x) {
    if (x$parse_string == "") {
      return(NULL)
    }
    samps <- do.call(
      .sample_trials,
      c(x$phase_info$general_info, list(
        masterlist = design@mapping$trial_names,
        miniblocks = miniblocks
      ))
    )
    c(list(model = model, group = x$group, phase = x$phase), samps)
  })
  # finally, convert lists to data.frames and bind across phases per group
  gs <- unlist(lapply(des, "[[", "group"))
  ugs <- unique(gs)

  lapply(ugs, function(g) {
    d <- do.call(rbind, lapply(samples[which(gs == g)], as.data.frame))
    d$trial <- seq_len(nrow(d))
    d
  })
}

.sample_trials <- function(
    trial_names, is_test,
    trial_repeats, randomize,
    miniblocks, masterlist, ...) {
  block_size <- 1
  # do miniblocks if necessary
  if (length(trial_repeats) > 1 && miniblocks) {
    gcd <- Reduce(.gcd, trial_repeats)
    per_block <- trial_repeats / gcd
    block_size <- sum(per_block)
    tps <- c() # note the redefining
    tstps <- c()
    for (b in 1:gcd) {
      ts <- unlist(lapply(
        seq_along(trial_names),
        function(n) rep(which(masterlist %in% trial_names[n]), per_block[n])
      ))
      tsts <- unlist(lapply(
        seq_along(trial_names),
        function(n) rep(is_test[n], per_block[n])
      ))
      # randomize if necessary
      if (randomize) {
        ri <- sample(length(ts))
        ts <- ts[ri]
        tsts <- tsts[ri]
      }
      tps <- c(tps, ts)
      tstps <- c(tstps, tsts)
    }
  } else {
    tps <- unlist(lapply(
      seq_along(trial_names),
      function(n) rep(which(masterlist %in% trial_names[n]), trial_repeats[n])
    ))
    tstps <- unlist(lapply(
      seq_along(trial_names),
      function(n) rep(is_test[n], trial_repeats[n])
    ))
    # randomize if necessary
    if (randomize) {
      ri <- sample(length(tps))
      tps <- tps[ri]
      tstps <- tstps[ri]
    }
  }
  return(list(
    tp = tps,
    tn = masterlist[tps],
    is_test = tstps,
    block_size = block_size
  ))
}

.augment_experience <- function(
    exper, model,
    design, parameters, timings, ...) {
  if (model == "ANCCR") {
    exper <- .anccrize_experience(
      exper, design,
      parameters, timings, ...
    )
  }
  if (model == "TD") {
    exper <- .tdrize_experience(
      exper, design, parameters, timings, ...
    )
  }
  exper
}

# function to return the gcd
.gcd <- function(x, y) {
  r <- x %% y
  return(ifelse(r, .gcd(y, r), y))
}

# a function to evaluate an expression with a specific seed
.with_seed <- function(seed, expr) {
  if (!is.null(seed)) {
    expr <- substitute(expr)
    # get original seed if it exists
    if (exists(".Random.seed")) {
      oseed <- .Random.seed
      # reinstate the seed on exit
      on.exit(.Random.seed <<- oseed)
    }
    set.seed(seed)
    eval.parent(expr)
  } else {
    eval.parent(expr)
  }
}
