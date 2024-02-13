#' An assorment of functions to help parse models

# experiment is a CalmrExperiment
# returns a list
.parse_experiment <- function(experiment) {
  n <- length(experiment)
  sapply(seq_len(n), function(r) {
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
  gen_dat$trial <- seq_len(nrow(gen_dat))
  gen_dat$trial_type <- args$mapping[[1]]$trial_names[gen_dat$tp]
  gen_dat <- dplyr::select(gen_dat, -"tp")
  need_enframe <- c(
    "es", "vs", "evs", "ivs",
    "acts", "relacts", "rs", "os"
  )

  dat <- NULL
  # arrays that need to be enframed
  if (type %in% need_enframe) {
    # special treatment for HeiDI acts (ugly)
    if (args$model %in% c("HDI2020", "HD2022") && type == "acts") {
      combs <- tibble::enframe(lapply(raw$combvs, function(x) {
        as.data.frame(as.table(x), stringsAsFactors = FALSE)
      }), name = "trial")
      combs$act_type <- "comb"
      chains <- tibble::enframe(lapply(raw$chain, function(x) {
        as.data.frame(as.table(x), stringsAsFactors = FALSE)
      }), name = "trial")
      chains$act_type <- "chain"
      full_dat <- dplyr::bind_rows(
        dplyr::left_join(gen_dat, combs, by = "trial"),
        dplyr::left_join(gen_dat, chains, by = "trial")
      )
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
  }
  if (type %in% c("as")) {
    dat <- as.data.frame(raw)
    long_cols <- names(dat)
    full_dat <- cbind(gen_dat, dat)
    full_dat <- tidyr::pivot_longer(full_dat,
      cols = long_cols, names_to = "s1"
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
    agg_dat[m] <- sapply(outputs, function(o) {
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
  # WORKING HERE
  browser()
}

.agg <- function(res, type) {
  dat <- NULL
  if (type == "es") {
    dat <- do.call("rbind", res$es) %>%
      dplyr::group_by(
        .data$group, .data$trial, .data$trial_type, .data$phase,
        .data$s1, .data$s2, .data$block_size
      ) %>% # summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        group = as.factor(.data$group), s1 = as.factor(.data$s1),
        s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
        phase = as.factor(.data$phase)
      )
  }
  if (type == "vs") {
    dat <- do.call("rbind", res[[type]]) %>%
      dplyr::group_by(
        .data$group, .data$trial, .data$trial_type, .data$phase,
        .data$s1, .data$s2, .data$block_size
      ) %>% # summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        group = as.factor(.data$group), s1 = as.factor(.data$s1),
        s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
        phase = as.factor(.data$phase)
      )
  }
  if (type == "eivs") {
    ev <- do.call("rbind", res$evs) %>%
      dplyr::group_by(
        .data$group, .data$trial, .data$trial_type, .data$phase,
        .data$s1, .data$s2, .data$block_size
      ) %>% # summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        group = as.factor(.data$group), s1 = as.factor(.data$s1),
        s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
        phase = as.factor(.data$phase), assoc_type = "Excitatory"
      )
    iv <- do.call("rbind", res$ivs) %>%
      dplyr::group_by(
        .data$group, .data$trial, .data$trial_type, .data$phase,
        .data$s1, .data$s2, .data$block_size
      ) %>% # summarize
      dplyr::summarise(value = -mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        group = as.factor(.data$group), s1 = as.factor(.data$s1),
        s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
        phase = as.factor(.data$phase), assoc_type = "Inhibitory"
      )
    net <- ev
    net$value <- net$value + iv$value
    net$assoc_type <- "Net"
    dat <- rbind(ev, iv, net)
  }
  if (type == "as") {
    dat <- do.call("rbind", res$as) %>%
      dplyr::group_by(
        .data$group, .data$trial, .data$phase,
        .data$trial_type, .data$s1, .data$block_size
      ) %>% # summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
        s1 = as.factor(.data$s1), phase = as.factor(.data$phase)
      )
  }
  if (type == "acts") {
    if (any(res$model %in% c("HD2022", "HDI2020"))) {
      dat <- do.call("rbind", res$acts) %>%
        dplyr::group_by(
          .data$group, .data$trial, .data$phase, .data$trial_type,
          .data$act_type, .data$s1, .data$s2, .data$block_size
        ) %>%
        dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
        dplyr::mutate(
          group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
          act_type = as.factor(.data$act_type), s1 = as.factor(.data$s1),
          s2 = as.factor(.data$s2), phase = as.factor(.data$phase)
        )
    } else {
      dat <- do.call("rbind", res$acts) %>%
        dplyr::group_by(
          .data$group, .data$trial, .data$phase, .data$trial_type,
          .data$s1, .data$s2, .data$block_size
        ) %>%
        dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
        dplyr::mutate(
          group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
          s1 = as.factor(.data$s1),
          s2 = as.factor(.data$s2), phase = as.factor(.data$phase)
        )
    }
  }
  if (type == "relacts") {
    dat <- do.call("rbind", res$relacts) %>%
      dplyr::group_by(
        .data$group, .data$trial, .data$phase, .data$trial_type,
        .data$s1, .data$s2, .data$block_size
      ) %>%
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
        s1 = as.factor(.data$s1),
        s2 = as.factor(.data$s2), phase = as.factor(.data$phase)
      )
  }
  if (type == "rs") {
    dat <- do.call("rbind", res$rs) %>%
      dplyr::group_by(
        .data$group, .data$trial, .data$phase, .data$trial_type,
        .data$s1, .data$s2, .data$block_size
      ) %>% # summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
        s1 = as.factor(.data$s1), s2 = as.factor(.data$s2),
        phase = as.factor(.data$phase)
      )
  }
  if (type == "os") {
    dat <- do.call("rbind", res$os) %>%
      dplyr::group_by(
        .data$group, .data$trial, .data$phase, .data$trial_type,
        .data$s1, .data$comp, .data$s2, .data$block_size
      ) %>%
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
        s1 = as.factor(.data$s1),
        comp = as.factor(.data$comp),
        s2 = as.factor(.data$s2), phase = as.factor(.data$phase)
      )
  }
  dat
}

filter_calmr_results <- function(parsed_experiment, filters) {
  if (!is.null(parsed_experiment)) {
    parsed_experiment@parsed_results <- lapply(parsed_experiment@parsed_results, function(x) x %>% dplyr::filter(.data$phase %in% filters$phase & .data$trial_type %in% filters$trial_type))
  }
  parsed_experiment
}
