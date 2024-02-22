#' Make CalmrExperiment
#'
#' @description Makes a CalmrExperiment object containing
#' the arguments necessary to run models
#' @param design A design data.frame
#' @param parameters Parameters for a  model as
#' returned by `get_parameters`
#' @param model A string specifying the model name. One of `supported_models()`
#' @param options A list with options as returned by `get_exp_opts`
#' @param ... Extra parameters passed to other functions
#' @return A CalmrExperiment object
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
    model = NULL, options = get_exp_opts(), ...) {
  # parse design
  design <- parse_design(design,
    model = model, ...
  )

  .calmr_assert("length", 1, model = model)

  # assert model
  model <- .calmr_assert("supported_model", model)

  # assert parameters
  parameters <- .calmr_assert("parameters", parameters,
    design = design, model = model
  )
  # assert options
  options <- .calmr_assert("experiment_options", options)

  # build the experiment
  arguments <- .build_arguments(
    design = design,
    model = model,
    options = options,
    ...
  )
  # add mapping
  arguments$mapping <- list(design@mapping)
  # add parameters
  arguments$parameters <- list(parameters)

  return(methods::new("CalmrExperiment",
    arguments = arguments, design = design
  ))
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

.build_arguments <- function(design, options, model, ...) {
  args <- .build_experiment(
    design = design,
    model = model, options = options
  )
  if (model %in% c("ANCCR")) {
    args <- .anccrize_arguments(args, ...)
  }
  args
}

# general function to build experiment
.build_experiment <- function(design, model, options) {
  # sample trials
  exptb <- design@design |>
    tidyr::expand_grid(iteration = 1:options$iterations)

  exptb$samples <- apply(exptb, 1, function(x) {
    do.call(
      .sample_trials,
      c(x$phase_info$general_info, list(
        randomize = x$randomize,
        masterlist = design@mapping$trial_names,
        miniblocks = options$miniblocks
      ))
    )
  })

  # add phaselab and block information
  exptb <- tidyr::unnest_wider(exptb, "samples")
  exptb$phaselab <- apply(exptb, 1, function(x) rep(x$phase, length(x$tps)),
    simplify = FALSE
  )
  exptb$blocks <- apply(exptb, 1, function(x) rep(x$block_size, length(x$tps)),
    simplify = FALSE
  )

  # one last manipulation to concatenate phases into single rows
  exptb <- exptb %>%
    dplyr::group_by(.data$iteration, .data$group) |>
    dplyr::summarise(
      tp = list(unlist(.data$tps)),
      tn = list(unlist(.data$tns)),
      is_test = list(unlist(.data$is_test)),
      phase = list(unlist(.data$phaselab)),
      block_size = list(unlist(.data$blocks))
    )

  # bundle into experiences
  experience <- apply(
    exptb[c(-1)], 1,
    function(x) do.call("cbind.data.frame", x)
  )

  tibble::tibble(
    model,
    exptb[, c("iteration", "group")],
    experience
  )
}

# function to return the gcd
.gcd <- function(x, y) {
  r <- x %% y
  return(ifelse(r, .gcd(y, r), y))
}

# Aguments design depending on the model
augment_design <- function(design, model, ...) {
  if (model %in% c("ANCCR")) {
    # creates eventlogs
    design <- .anccrize_design(design, ...)
  }
  design
}
