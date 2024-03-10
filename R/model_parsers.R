#' @import data.table

# Parses model raw results
# args is the list of arguments used to fit the model
# raw are the raw results
# returns a list
.parse_model <- function(raw, experience, model) {
  outputs <- model_outputs(model)
  sapply(outputs, function(o) {
    .parse_raw_data_table(raw[[o]],
      type = o,
      experience = experience,
      model = model
    )
  }, simplify = FALSE)
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
  names(raw)[names(raw) == "V1"] <- "tie"
  raw
}

.parse_raw_data_table <- function(raw, type, experience, model) {
  # local bindings
  tie <- NULL

  # outputs containing three dimensional arrays (trial, s, s)
  threes <- c(
    "es", "vs", "eivs",
    "acts", "relacts", "rs", "os",
    "m_ij", "ncs", "anccrs", "cws",
    "psrcs", "das", "qs", "ps"
  )
  # get general data
  gen_dat <- data.table::as.data.table(experience)
  # necessary for join
  gen_dat[, "tie" := seq_len(nrow(gen_dat))]
  if (!is.list(raw)) {
    raw2d <- data.table::as.data.table(raw)
    if (length(dim(raw)) > 2) {
      raw2d[, "tie" := rep(seq_len(nrow(gen_dat)),
        each = prod(dim(raw)[-1])
      )]
    }
  } else {
    if (model %in% c("HDI2020", "HD2022") && type == "acts") {
      raw2d <- data.table::rbindlist(
        sapply(names(raw), function(r) {
          hold <- .unnest_raw_list(raw[[r]])
          hold[, "type" := r]
        }, simplify = FALSE)
      )
    } else {
      raw2d <- data.table::rbindlist(
        sapply(names(raw), function(r) {
          hold <- data.table::as.data.table(raw[[r]])
          hold[, "type" := r]
          if (length(dim(raw[[r]]) > 3)) {
            hold[, "tie" := rep(seq_len(nrow(gen_dat)),
              each = prod(dim(raw[[r]])[-1])
            )]
          }
        }, simplify = FALSE)
      )
    }
  }
  if (type %in% threes) {
    # now join
    full_dat <- gen_dat[raw2d, on = list(tie)]
    # renaming
    if (type %in% c("os")) {
      # the only model output that does not follow Var1 = s1, Var2 = s2
      full_dat <- .rename(full_dat, c("V2", "V3", "V4"), c("s1", "comp", "s2"))
    } else {
      full_dat <- .rename(full_dat, c("V2", "V3"), c("s1", "s2"))
    }
  } else {
    # need to melt, but no need to name
    full_dat <- cbind(gen_dat, raw2d)
    full_dat <- data.table::melt(full_dat,
      id.vars = names(gen_dat),
      measure.vars = names(raw),
      variable.name = "s1"
    )
  }
  # renaming
  full_dat <- .rename(full_dat, "tn", "trial_type")
  # get rid of tie columns
  full_dat <- full_dat[, -c("tie")]
  # get rid of V1 if around
  if ("V1" %in% names(full_dat)) {
    full_dat <- full_dat[, -c("V1")]
  }

  full_dat
}

# experiment is a CalmExperiment
# returns a list of tibbles
.aggregate_experiment <- function(
    experiment,
    .callback_fn = NULL, ...) {
  # Aggregation is done on a model by model basis
  models <- unique(experiment@model)
  agg_dat <- list()
  for (m in models) {
    outputs <- model_outputs(m)
    mod_dat <- experiment@results@parsed_results[
      experiment@.model == m
    ]
    pb <- progressr::progressor(length(outputs))
    agg_dat[[m]] <- sapply(outputs, function(o) {
      pb(message = sprintf("Aggregating model %s", m))
      if (!is.null(.callback_fn)) .callback_fn()
      # put data together
      big_dat <- data.table::rbindlist(lapply(mod_dat, "[[", o))
      # aggregate
      agg_dat <- .aggregate_results_data_table(big_dat, type = o)
      agg_dat$model <- m
      agg_dat
    }, simplify = FALSE)
  }
  agg_dat
}

# type is the type of data
.aggregate_results_data_table <- function(dat, type) {
  value <- NULL # local binding
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

.rename <- function(x, from, to) {
  onames <- names(x)
  onames[onames %in% from] <- to
  names(x) <- onames
  x
}
