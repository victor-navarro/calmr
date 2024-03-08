#' Make CalmExperiment
#'
#' @description Makes a CalmExperiment object containing
#' the arguments necessary to run models
#' @param design A design data.frame
#' @param parameters Parameters for a  model as
#' returned by `get_parameters`
#' @param model A string specifying the model name. One of `supported_models()`
#' @param options A list with options as returned by `get_exp_opts`
#' @param .callback_fn A function for keeping track of progress. Internal use.
#' @param ... Extra parameters passed to other functions
#' @return A CalmExperiment object
#' @seealso \code{\link{parse_design}},
#' \code{\link{get_parameters}}, \code{\link{get_exp_opts}}
#' @examples
#' des <- data.frame(Group = "G1", P1 = "10A>(US)", R1 = TRUE)
#' ps <- get_parameters(des, model = "HD2022")
#' op <- get_exp_opts(miniblocks = TRUE, iterations = 2)
#' make_experiment(
#'   design = des, parameters = ps,
#'   model = "HD2022", options = op
#' )
#'
#' @export
#' @importFrom rlang .data

make_experiment <- function(
    design, parameters = NULL,
    model = NULL,
    options = get_exp_opts(),
    .callback_fn = NULL,
    ...) {
  # parse design
  design <- parse_design(design,
    model = model, ...
  )
  .calm_assert("length", 1, model = model)
  # assert model
  model <- .calm_assert("supported_model", model)
  # assert parameters
  parameters <- .calm_assert("parameters", parameters,
    design = design, model = model
  )
  # assert options
  options <- .calm_assert("experiment_options", options)

  # build the arguments for the experiment
  iter <- options$iterations
  pb <- progressr::progressor(iter)
  .parallel_standby(pb) # print parallel backend message
  pb(amount = 0, message = "Building experiment")
  arguments <- future.apply::future_lapply(seq_len(iter), function(x) {
    if (!is.null(.callback_fn)) .callback_fn() # for shiny
    args <- .build_experiment(
      design = design,
      model = model,
      options = options,
      ...
    )
    browser() # we go differently now
    # add mapping
    args$mapping <- list(design@mapping)
    # add parameters
    args$parameters <- list(parameters)
    # augment arguments if necessary
    args <- .augment_arguments(args, ...)
    pb("Building experiment")
    args
  }, future.seed = TRUE)

  arguments <- tibble::enframe(arguments, name = "iteration") |>
    tidyr::unnest("value")

  return(methods::new("CalmExperiment",
    arguments = arguments, design = design
  ))
}

# general function to build experiment
.build_experiment <- function(
    design, model, options,
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
        miniblocks = options$miniblocks
      ))
    )
    c(list(model = model, group = x$group, phase = x$phase), samps)
  })
  # finally, convert lists to data.frames and bind across phases per group
  gs <- unlist(lapply(des, "[[", "group"))
  experience <- lapply(unique(gs), function(g) {
    do.call(rbind, lapply(samples[which(gs == g)], as.data.frame))
  })
  # return as list
  list(
    model = rep(model, length(gs)),
    group = gs, experience = experience
  )
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
    tps = tps,
    tns = masterlist[tps],
    is_test = tstps,
    block_size = block_size
  ))
}

.augment_arguments <- function(args, ...) {
  if (unique(args$model) == "ANCCR") {
    args <- .anccrize_arguments(args, ...)
  }
  args
}

# function to return the gcd
.gcd <- function(x, y) {
  r <- x %% y
  return(ifelse(r, .gcd(y, r), y))
}
