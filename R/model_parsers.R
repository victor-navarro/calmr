#' @import data.table
#' @noRd
# unnests a list containing named lists of ragged arrays
.unnest_nested_raw_list <- function(raw) {
  data.table::rbindlist(lapply(seq_along(raw), function(tr) {
    stims <- names(raw[[tr]])
    do.call(rbind, lapply(stims, function(entry) {
      holder <- data.table::data.table(
        tie = tr,
        V0 = entry,
        as.table(raw[[tr]][[entry]])
      )
      # hack to shift names because V1 is not the trial
      names(holder) <- c("tie", paste0("V", 2:(2 + ncol(holder) - 2)))
      holder
    }))
  }))
}

# ^look above for a perhaps slower, but much simpler implementation
.unnest_raw_list <- function(raw) {
  dims <- lapply(raw, dim)
  udims <- unique(dims)
  matches <- lapply(udims, function(u) {
    which(unlist(lapply(dims, function(d) all(d == u))))
  })
  # collapse into 2d array going through each unique dimensionality
  # really need to keep an eye on this function
  # when applying to models other than HeiDI
  # it creates many problems that will go undetected
  raw <- data.table::rbindlist(lapply(matches, function(m) {
    hold <- data.table::as.data.table(aperm(simplify2array(raw[m]), c(3, 1, 2)))
    hold[, "V2" := unlist(lapply(
      raw[m],
      function(i) rep(rownames(i), ncol(i))
    ))]
  }))
  # now need to put the trials back
  # create the trial pointers according to matches and their dimensionality
  tps <- c(unlist(sapply(seq_along(matches), function(d) {
    rep(matches[[d]], each = prod(udims[[d]]))
  })))
  # put them into the data
  raw[, "V1" := tps] # nolint
  names(raw)[names(raw) == "V1"] <- "tie" # rename
  raw
}

# for outputs containing ND arrays (e.g., trial, s, s)
.parse_nd <- function(object, type) {
  # local bindings
  tie <- NULL
  # generic data
  gen_dat <- .get_gen_dat(object)
  # join with general data
  raw2d <- .make_raw_2d(object@.last_raw_results[[type]], gen_dat)
  full_dat <- gen_dat[raw2d, on = list(tie)]
  # post process
  full_dat <- .post_process_data_table(full_dat, object, type)
  full_dat
}


.parse_typed_ragged <- function(object, type) {
  # local bindings
  tie <- NULL
  # generic data
  gen_dat <- .get_gen_dat(object)
  # join with general data
  raw2d <- data.table::rbindlist(
    sapply(names(object@.last_raw_results[[type]]), function(r) {
      hold <- .unnest_raw_list(object@.last_raw_results[[type]][[r]])
      hold[, "type" := r]
    }, simplify = FALSE)
  )
  full_dat <- gen_dat[raw2d, on = list(tie)]
  # post process
  full_dat <- .post_process_data_table(full_dat, object, type)
  full_dat
}

.parse_nested_ragged <- function(object, type) {
  # local bindings
  tie <- NULL
  # generic data
  gen_dat <- .get_gen_dat(object)
  # join with general data
  raw2d <- .unnest_nested_raw_list(object@.last_raw_results[[type]])
  full_dat <- gen_dat[raw2d, on = list(tie)]
  # post process
  full_dat <- .post_process_data_table(full_dat, object, type)
  full_dat
}

.parse_typed <- function(object, type) {
  # local bindings
  tie <- NULL
  # generic data
  gen_dat <- .get_gen_dat(object)
  # join with general data
  raw2d <- data.table::rbindlist(
    sapply(names(object@.last_raw_results[[type]]), function(r) {
      hold <- data.table::as.data.table(object@.last_raw_results[[type]][[r]])
      hold[, "type" := r]
      if (length(dim(object@.last_raw_results[[type]][[r]])) > 2) {
        ties <- rep(seq_len(nrow(gen_dat)),
          each = prod(dim(object@.last_raw_results[[type]][[r]])[-1])
        )
        hold[, "tie" := ties]
      }
      hold
    }, simplify = FALSE)
  )
  full_dat <- gen_dat[raw2d, on = list(tie)]
  # post process
  full_dat <- .post_process_data_table(full_dat, object, type)
  full_dat
}

# for outputs containing 2D arrays (e.g., trial, s)
.parse_2d <- function(object, type) {
  # generic data
  gen_dat <- .get_gen_dat(object)
  raw2d <- data.table::as.data.table(object@.last_raw_results[[type]])
  # need to melt, but no need to name
  full_dat <- cbind(gen_dat, raw2d)
  full_dat <- data.table::melt(full_dat,
    id.vars = names(gen_dat),
    measure.vars = names(object@.last_raw_results[[type]]),
    variable.name = "s1"
  )
  # post process
  full_dat <- .post_process_data_table(full_dat, object, type)
  full_dat
}

.post_process_data_table <- function(dat, object, type) {
  # renaming
  dat <- .name_data(dat, object, type)
  # get rid of tie columns
  if ("tie" %in% names(dat)) {
    dat <- dat[, -c("tie")]
  }
  # get rid of V1 if around
  if ("V1" %in% names(dat)) {
    dat <- dat[, -c("V1")]
  }
  # turn t_bin into a numeric
  if ("t_bin" %in% names(dat)) {
    dat$t_bin <- as.numeric(dat$t_bin)
  }
  dat
}

.name_data <- function(dat, object, type) {
  newnames <- object@.dnames_map[[type]]
  dat <- .rename(
    dat,
    paste0("V", seq_along(newnames) + 1),
    newnames
  )
  dat <- .rename(dat, "tn", "trial_type")
  dat
}

.get_gen_dat <- function(object) {
  gen_dat <- data.table::as.data.table(object@.last_experience)
  if (object@model_name == "TD") {
    # deal with experiences for the TD model
    gen_dat[, c(
      "stimulus", "time", "rtime",
      "duration", "b_from", "b_to"
    ) := NULL]
    gen_dat <- unique(gen_dat)
  }
  gen_dat[, "tie" := seq_len(nrow(gen_dat))]
  gen_dat
}

.make_raw_2d <- function(raw, gen_dat) {
  raw2d <- data.table::as.data.table(raw)
  raw2d[, "tie" := rep(
    seq_len(nrow(gen_dat)),
    each = prod(dim(raw)[-1])
  )]
  raw2d
}

#' Aggregate CalmrExperiment results
#' @param experiment A CalmrExperiment object
#' @param outputs A character vector specifying which outputs to aggregate
#' @param .callback_fn A function to call on each succesful aggregation.
#' @return A CalmrExperiment object
#' @noRd
.aggregate_experiment <- function(
    experiment, outputs,
    .callback_fn = NULL) {
  # throw error if outputs requested are not in parsed_results
  # or if parsed_results do not exist
  res <- parsed_results(experiment)
  if (
    !all(sapply(
      res,
      function(i) all(outputs %in% names(i))
    )) ||
      is.null(res)) {
    stop(c(
      "Cannot aggregate requested outputs ",
      "because some do not exist in object@results@parsed_results. ",
      "Use `parse` on your object with the appropriate outputs first."
    ))
  }

  pb <- progressr::progressor(length(outputs))
  agg_dat <- list()
  agg_dat[[experiment@model]] <- sapply(outputs, function(o) {
    pb(message = sprintf("Aggregating model %s", experiment@model))
    if (!is.null(.callback_fn)) .callback_fn() # nocov
    # put data together
    big_dat <- data.table::rbindlist(lapply(res, "[[", o))
    # aggregate
    agg_dat <- .aggregate_results_data_table(big_dat,
      experiment@models[[1]], # use the first model
      type = o
    )
    agg_dat$model <- experiment@model
    agg_dat
  }, simplify = FALSE)
  agg_dat
}

# type is the type of data
.aggregate_results_data_table <- function(dat, object, type) {
  value <- time <- NULL # local binding
  common_terms <- c("group", "phase", "trial_type", "trial", "block_size", "s1")
  form <- c(common_terms, object@.formula_map[[type]])
  dat <- data.table::data.table(dat)
  if (!("time" %in% names(dat))) {
    data.table::setDT(dat)[, list("value" = mean(value)), by = form]
  } else {
    data.table::setDT(dat)[, lapply(.SD, mean, na.rm = TRUE),
      by = form, .SDcols = c("value", "time")
    ]
  }
}

.rename <- function(x, from, to) {
  onames <- names(x)
  onames[onames %in% from] <- to
  names(x) <- onames
  x
}
