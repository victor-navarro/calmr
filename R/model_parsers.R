# experiment is a CalmrExperiment
# returns a list
.parse_experiment <- function(experiment) {
  n <- length(experiment)
  pb <- progress::progress_bar$new(
    format = "Parsing Results [:bar] :current/:total (:percent)",
    total = n
  )
  sapply(seq_len(n), function(r) {
    pb$tick()
    model <- experiment@arguments$model[r]
    outputs <- model_outputs(model)
    sapply(outputs, function(o) {
      raw <- experiment@results@raw_results[[r]][[o]]
      .parse_raw(raw, type = o, args = experiment@arguments[r, ])
    }, simplify = FALSE)
  }, simplify = FALSE)
}

# raw is a list
# type is the type of list (depends on model)
# args is a tbl with the arguments (make_experiment)
.parse_raw <- function(raw, type, args) {
  full_dat <- NULL
  # get general data
  gen_dat <- args$experience[[1]]
  gen_dat$trial_type <- args$mapping[[1]]$trial_names[gen_dat$tp]
  gen_dat <- dplyr::select(gen_dat, -"tp")
  need_enframe <- c(
    "es", "vs", "eivs",
    "acts", "relacts", "rs", "os",
    "m_ij", "ncs", "anccrs", "rews", "psrcs", "das", "qs"
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
  # labelling for Konorskian models
  if (type %in% c("eivs")) {
    full_dat$assoc_type <- ifelse(full_dat$type == "evs",
      "excitatory", "inhibitory"
    )
  }
  # labelling for ANCCR
  if (type %in% c("psrcs")) {
    full_dat$rep_type <- ifelse(
      full_dat$type == "src",
      "successor", "predecessor"
    )
  }

  full_dat
}

# experiment is a CalmrExperiment
# returns a list of tibbles
.aggregate_experiment <- function(experiment) {
  # Aggregation is done on a model by model basis
  models <- unique(experiment@arguments$model)
  agg_dat <- list()
  for (m in models) {
    outputs <- model_outputs(m)
    mod_dat <- experiment@results@parsed_results[
      experiment@arguments$model == m
    ]
    pb <- progress::progress_bar$new(
      format = "Aggregating results [:bar] :current/:total (:percent)",
      total = length(outputs)
    )
    agg_dat[[m]] <- sapply(outputs, function(o) {
      pb$tick()
      # put data together
      big_dat <- do.call(rbind, lapply(mod_dat, function(x) x[[o]]))
      # aggregate
      .aggregate_results(big_dat, type = o)
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
    "group", "trial", "trial_type",
    "phase", "s1", "s2", "block_size"
  )
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
