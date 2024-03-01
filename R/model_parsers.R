#' @import data.table

# Parses model raw results
# args is the tbl row used to fit the model
# raw are the raw results
# returns a list
.parse_model <- function(raw, args) {
  model <- args$model
  outputs <- model_outputs(model)
  sapply(outputs, function(o) {
    .parse_raw_data_table(raw[[o]],
      type = o,
      args = args
    )
  }, simplify = FALSE)
}

# raw is a list
# type is the type of list (depends on model)
# args is a tbl with the arguments (make_experiment)
.parse_raw <- function(raw, type, args) {
  full_dat <- NULL
  # get general data
  gen_dat <- args$experience
  need_enframe <- c(
    "es", "vs", "eivs",
    "acts", "relacts", "rs", "os",
    "m_ij", "ncs", "anccrs", "cws", "psrcs", "das", "qs", "ps"
  )

  dat <- NULL
  # arrays that need to be enframed
  if (type %in% need_enframe) {
    # special treatment for HeiDI acts
    # (list of lists with irregular matrices; ugly) and
    # models that return a bundled list of matrices
    # that return an output as a list
    if ((args$model %in% c("HDI2020", "HD2022") && type == "acts")) {
      raw_typed <- sapply(names(raw), function(r) {
        hold <- tibble::enframe(lapply(raw[[r]], function(x) {
          as.data.frame(as.table(x), stringsAsFactors = FALSE)
        }), name = "trial")
        hold$type <- r
        dplyr::left_join(gen_dat, hold, by = "trial")
      }, simplify = FALSE)
      full_dat <- dplyr::bind_rows(raw_typed)
    } else if (type %in% c("eivs", "psrcs")) {
      raw_typed <- sapply(names(raw), function(r) {
        hold <- tibble::enframe(apply(raw[[r]], 1, function(x) {
          as.data.frame(as.table(x), stringsAsFactors = FALSE)
        }), name = "trial")
        hold$type <- r
        dplyr::left_join(gen_dat, hold, by = "trial")
      }, simplify = FALSE)
      full_dat <- dplyr::bind_rows(raw_typed)
    } else {
      dat <- tibble::enframe(apply(raw, 1, function(x) {
        as.data.frame(as.table(x), stringsAsFactors = FALSE)
      }), name = "trial")
      # adding general data
      full_dat <- dplyr::left_join(gen_dat, dat, by = "trial")
    }
    # unnest
    full_dat <- tidyr::unnest(full_dat, cols = "value")

    # rename
    if (type %in% c("os")) {
      # the only model output that does not follow Var1 = s1, Var2 = s2
      full_dat <- dplyr::rename(full_dat,
        "s1" = "Var1", "comp" = "Var2", "s2" = "Var3", "value" = "Freq"
      )
    } else {
      full_dat <- dplyr::rename(full_dat,
        "s1" = "Var1", "s2" = "Var2", "value" = "Freq"
      )
    }
  } else {
    # outputs that do not need enframing
    dat <- as.data.frame(raw)
    long_cols <- names(dat)
    full_dat <- cbind(gen_dat, dat)
    full_dat <- tidyr::pivot_longer(full_dat,
      cols = tidyr::all_of(long_cols), names_to = "s1"
    )
  }
  # renaming
  full_dat <- dplyr::rename(full_dat, "trial_type" = "tn")
  full_dat
}

.unnest_raw <- function(raw) {
  raw <- data.table::as.data.table(raw)
  names(raw)[names(raw) == "V1"] <- "trial"
  raw
}

.unnest_raw_list <- function(raw) {
  dims <- lapply(raw, dim)
  udims <- unique(dims)
  matches <- lapply(udims, function(u) {
    which(unlist(lapply(dims, function(d) all(d == u))))
  })
  raw <- data.table::rbindlist(lapply(matches, function(m) {
    data.table::as.data.table(aperm(simplify2array(raw[m]), c(3, 1, 2)))
  }))
  # now need to put the trials back
  raw[, "V1" := rep(unlist(matches), sapply(dims, prod))]
  names(raw)[names(raw) == "V1"] <- "trial"
  raw
}

.parse_raw_data_table <- function(raw, type, args) {
  # outputs containing three dimensional arrays (trial, s, s)
  threes <- c(
    "es", "vs", "eivs",
    "acts", "relacts", "rs", "os",
    "m_ij", "ncs", "anccrs", "cws",
    "psrcs", "das", "qs", "ps"
  )
  # get general data
  gen_dat <- data.table::as.data.table(args$experience)
  if (!is.list(raw)) {
    raw <- .unnest_raw(raw)
  } else {
    if (args$model %in% c("HDI2020", "HD2022") && type == "acts") {
      raw <- data.table::rbindlist(
        sapply(names(raw), function(r) {
          hold <- .unnest_raw_list(raw[[r]])
          hold[, "type" := r]
        }, simplify = FALSE)
      )
    } else {
      raw <- data.table::rbindlist(
        sapply(names(raw), function(r) {
          hold <- .unnest_raw(raw[[r]])
          hold[, "type" := r]
        }, simplify = FALSE)
      )
    }
  }
  if (type %in% threes) {
    # can bind directly
    full_dat <- raw[gen_dat, on = list(trial)]
    # renaming
    if (type %in% c("os")) {
      # the only model output that does not follow Var1 = s1, Var2 = s2
      full_dat <- dplyr::rename(full_dat,
        "s1" = "V2", "comp" = "V3", "s2" = "V4"
      )
    } else {
      full_dat <- dplyr::rename(full_dat,
        "s1" = "V2", "s2" = "V3"
      )
    }
  } else {
    # need to melt, but no need to name
    full_dat <- cbind(gen_dat, raw)
    full_dat <- data.table::melt(full_dat,
      id.vars = names(gen_dat),
      measure.vars = names(raw),
      variable.name = "s1"
    )
  }
  # renaming
  full_dat <- dplyr::rename(full_dat, "trial_type" = "tn")
  full_dat
}

# experiment is a CalmrExperiment
# returns a list of tibbles
.aggregate_experiment <- function(
    experiment,
    .callback_fn = NULL, ...) {
  # Aggregation is done on a model by model basis
  models <- unique(experiment@arguments$model)
  agg_dat <- list()
  for (m in models) {
    outputs <- model_outputs(m)
    mod_dat <- experiment@results@parsed_results[
      experiment@arguments$model == m
    ]
    pb <- progressr::progressor(length(outputs))
    agg_dat[[m]] <- sapply(outputs, function(o) {
      pb(message = sprintf("Aggregating model %s", m))
      if (!is.null(.callback_fn)) .callback_fn()
      # put data together
      big_dat <- do.call(rbind, lapply(mod_dat, function(x) x[[o]]))
      # aggregate
      .aggregate_results_data_table(big_dat, type = o)
    }, simplify = FALSE)
  }
  agg_dat
}

# dat is a tbl
# type is the type of data
.aggregate_results <- function(dat, type) {
  # define base terms for aggregation formula
  no_s2 <- c("as", "e_ij", "e_i", "m_i", "delta")
  terms <- c(
    "phase", "trial_type",
    "trial", "s1", "s2", "block_size"
  )
  if ("time" %in% names(dat)) {
    terms <- c(terms, "time")
  }
  if (type %in% no_s2) {
    terms <- terms[!(terms == "s2")]
  }
  if ("type" %in% names(dat)) {
    terms <- c(terms, "type")
  }
  if (type %in% c("os")) {
    terms <- c(terms, "comp")
  }
  form <- formula(paste0("value~", paste0(terms, collapse = "+")))
  aggregate(form, dat, mean)
}

# dat is a tbl
# type is the type of data
.aggregate_results_data_table <- function(dat, type) {
  dat <- data.table::data.table(dat)
  # define base terms for aggregation formula
  no_s2 <- c("as", "e_ij", "e_i", "m_i", "delta")
  terms <- c(
    "group", "phase", "trial_type",
    "trial", "s1", "s2", "block_size"
  )
  if ("time" %in% names(dat)) {
    terms <- c(terms, "time")
  }
  if (type %in% no_s2) {
    terms <- terms[!(terms == "s2")]
  }
  if ("type" %in% names(dat)) {
    terms <- c(terms, "type")
  }
  if (type %in% c("os")) {
    terms <- c(terms, "comp")
  }
  form <- paste0(terms, collapse = ",")
  data.table::setDT(dat)[, list("value" = mean(value)), by = form]
}


filter_calmr_results <- function(parsed_experiment, filters) {
  if (!is.null(parsed_experiment)) {
    parsed_experiment@parsed_results <-
      lapply(
        parsed_experiment@parsed_results,
        function(x) {
          x %>% dplyr::filter(
            .data$phase %in% filters$phase &
              .data$trial_type %in% filters$trial_type
          )
        }
      )
  }
  parsed_experiment
}
