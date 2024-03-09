#' Make CalmExperiment
#'
#' @description Makes a CalmExperiment object containing
#' the arguments necessary to run models
#' @param design A design data.frame
#' @param parameters Parameters for a  model as
#' returned by `get_parameters`
#' @param model A string specifying the model name. One of `supported_models()`
#' @param iterations An integer specifying the number of iterations per group.
#' @param miniblocks Whether to organize trials in miniblocks.
#' @param .callback_fn A function for keeping track of progress. Internal use.
#' @param ... Extra parameters passed to other functions
#' @return A CalmExperiment object
#' @seealso \code{\link{parse_design}},
#' @note The miniblocks option will direct the sampling function to create
#' equally-sized miniblocks with random trials within a phase.
#' For example, the phase string "2A/2B" will create two miniblocks
#' with one of each trial. The phase string "2A/4B" will create two miniblocks
#'  with one A trial, and 2 B trials. However, the phase string "2A/1B" will
#'  not result in miniblocks, even if miniblocks here is set to TRUE.
#' @examples
#' des <- data.frame(Group = "G1", P1 = "10A>(US)", R1 = TRUE)
#' ps <- get_parameters(des, model = "HD2022")
#' make_experiment(
#'   design = des, parameters = ps,
#'   model = "HD2022", iterations = 2
#' )
#'
#' @export
#' @importFrom rlang .data

make_experiment <- function(
    design, parameters = NULL,
    model = NULL,
    iterations = 1,
    miniblocks = TRUE,
    .callback_fn = NULL,
    ...) {
  # parse design
  design <- parse_design(design,
    model = model, ...
  )
  group_names <- design@raw_design[, 1]

  .calm_assert("length", 1, model = model)
  # assert model
  model <- .calm_assert("supported_model", model)
  # assert parameters
  parameters <- .calm_assert("parameters", parameters,
    design = design, model = model
  )

  # build the experiences for the experiment
  pb <- progressr::progressor(iterations)
  .parallel_standby(pb) # print parallel backend message
  pb(amount = 0, message = "Building experiment")
  allexps <- future.apply::future_sapply(seq_len(iterations), function(x) {
    if (!is.null(.callback_fn)) .callback_fn() # for shiny
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
      parameters = parameters, ...
    )
    pb("Building experiment")
    exper
  }, simplify = FALSE, future.seed = TRUE)
  # unnest once
  allexps <- unlist(allexps, recursive = FALSE)
  # return experiment
  methods::new("CalmExperiment",
    design = design,
    model = model,
    groups = group_names,
    parameters = stats::setNames(rep(
      list(parameters),
      length(group_names)
    ), group_names),
    experiences = allexps,
    results = methods::new("CalmExperimentResult"),
    .model = rep(model, length(allexps)),
    .group = rep(group_names, iterations),
    .iter = rep(seq_len(iterations), each = length(group_names))
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
        randomize = x$randomize,
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
      ts <- unlist(sapply(
        seq_along(trial_names),
        function(n) rep(which(masterlist %in% trial_names[n]), per_block[n])
      ))
      tsts <- unlist(sapply(
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
    tps <- unlist(sapply(
      seq_along(trial_names),
      function(n) rep(which(masterlist %in% trial_names[n]), trial_repeats[n])
    ))
    tstps <- unlist(sapply(
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
    design, parameters, ...) {
  if (model == "ANCCR") {
    exper <- .anccrize_experience(
      exper, design,
      parameters, ...
    )
  }
  exper
}

# function to return the gcd
.gcd <- function(x, y) {
  r <- x %% y
  return(ifelse(r, .gcd(y, r), y))
}
