#' Model helpers
#' @description An assorment of functions to help models

gen_ss_weights <- function(stims, default_val = 0){
  mat = matrix(default_val, ncol = length(stims), nrow = length(stims)) #perhaps a diagonal with 1s? Would accommodate self-association but increases model complexity.
  rownames(mat) = stims
  colnames(mat) = stims
  return(mat)
}

.parse <- function(mod, type){
  #check if mod has the type
  if (!(type %in% names(mod@model_results))) stop(sprintf("Model does not contain '%s' in model results.", type))
  dat = NULL
  if (type == "es"){
    dat = tibble::enframe(apply(mod@model_results$es, 1, function(x) as.data.frame(as.table(x))), name = "trial") %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest(.data$value) %>%
      dplyr::mutate(Var1 = as.character(.data$Var1),
                    Var2 = as.character(.data$Var2)) %>%
      dplyr::filter(.data$Var1 != .data$Var2) %>%
      dplyr::rename(trial_type = .data$tp, s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq)
  }
  if (type %in% c("vs", "evs", "ivs")){
    dat = tibble::enframe(apply(mod@model_results[[type]], 1, function(x) as.data.frame(as.table(x))), name = "trial") %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest(.data$value) %>%
      dplyr::mutate(Var1 = as.character(.data$Var1),
                    Var2 = as.character(.data$Var2)) %>%
      dplyr::filter(.data$Var1 != .data$Var2) %>%
      dplyr::rename(trial_type = .data$tp, s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq)
  }
  if (type == "acts"){
    combs = tibble::enframe(lapply(mod@model_results$acts$combvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest(.data$value) %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                                   Var2 = as.character(.data$Var2)) %>%
      dplyr::rename(trial_type = .data$tp, s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq) %>% dplyr::mutate(act_type = 'comb')

    chains = tibble::enframe(lapply(mod@model_results$acts$chainvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest(.data$value) %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                                   Var2 = as.character(.data$Var2)) %>%
      dplyr::rename(trial_type = .data$tp, s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq) %>% dplyr::mutate(act_type = 'chain')
    dat = rbind(combs, chains)
  }
  if (type == "as"){
    dat = as.data.frame(mod@model_results$as) %>%
      dplyr::mutate(trial = 1:dplyr::n()) %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::pivot_longer(cols = -c(.data$trial, .data$group, .data$is_test, .data$tp,
                                    .data$phase, .data$block_size), names_to = "s1") %>%
      dplyr::rename(trial_type = .data$tp)
  }
  if (type == "rs"){
    dat = tibble::enframe(apply(mod@model_results$rs, 1, function(x) as.data.frame(as.table(x))), name = "trial") %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest(.data$value) %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                                   Var2 = as.character(.data$Var2)) %>%
      dplyr::rename(trial_type = .data$tp, s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq)
  }
  dat
}

parse_model <- function(model){
  toparse = names(model@model_results)
  for (p in toparse){
    model@model_results[[p]] = .parse(model, p)
  }
  model
}

parse_experiment_results <- function(experiment, aggregate = T){
  #expects a tibble with one row per group
  #returns a list with all the relevant data for exporting (and plotting)
  if (!experiment@is_parsed){
    experiment@results = experiment@results %>% dplyr::rowwise() %>%
      dplyr::mutate(parsed_mod_responses = list(parse_model(.data$mod_data)@model_results))
    experiment@is_parsed = TRUE
  }

  if (aggregate){
    experiment@parsed_results = aggregate_experiment_results(experiment)
  }
  experiment
}

aggregate_experiment_results <- function(parsed_experiment){
  if (!parsed_experiment@is_parsed) stop("Experiment is not parsed.")
  agg = list()
  vars = names(parsed_experiment@results$parsed_mod_responses[[1]])
  dat = parsed_experiment@results %>%
    tidyr::unnest_wider(.data$parsed_mod_responses) %>%
    dplyr::ungroup()
  for (t in vars){
    agg[[t]] = .aggregate_results(dat, t)
  }
  agg
}

.aggregate_results <- function(res, type){
  dat = NULL
  if (type == "es"){
    dat = do.call("rbind", res$es) %>%
      dplyr::group_by(.data$group, .data$trial, .data$trial_type, .data$phase,
                      .data$s1, .data$s2, .data$block_size) %>% #summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(group = as.factor(.data$group), s1 = as.factor(.data$s1),
                    s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
                    phase = as.factor(.data$phase))
  }
  if (type %in% c("vs", "evs", "ivs")){
    dat = do.call("rbind", res[[type]]) %>%
      dplyr::group_by(.data$group, .data$trial, .data$trial_type, .data$phase,
                      .data$s1, .data$s2, .data$block_size) %>% #summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(group = as.factor(.data$group), s1 = as.factor(.data$s1),
                    s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
                    phase = as.factor(.data$phase))
  }
  if (type == "as"){
    dat = do.call("rbind", res$as) %>%
      dplyr::group_by(.data$group, .data$trial, .data$phase,
                      .data$trial_type, .data$s1, .data$block_size) %>% #summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
                    s1 = as.factor(.data$s1), phase = as.factor(.data$phase))
  }
  if (type == "acts"){
    dat = do.call("rbind", res$acts) %>%
      dplyr::group_by(.data$group, .data$trial, .data$phase, .data$trial_type,
                      .data$act_type, .data$s1, .data$s2, .data$block_size) %>%
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
                    act_type = as.factor(.data$act_type), s1 = as.factor(.data$s1),
                    s2 = as.factor(.data$s2), phase = as.factor(.data$phase))
  }
  if (type == "rs"){
    dat = do.call("rbind", res$rs) %>%
      dplyr::group_by(.data$group, .data$trial, .data$phase, .data$trial_type,
                      .data$s1, .data$s2, .data$block_size) %>% #summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
                    s1 = as.factor(.data$s1), s2 = as.factor(.data$s2),
                    phase = as.factor(.data$phase))
  }
  dat
}

filter_calmr_results <- function(parsed_experiment, filters){
  if (!is.null(parsed_experiment)){
    parsed_experiment@parsed_results = lapply(parsed_experiment@parsed_results, function(x) x %>% dplyr::filter(.data$phase %in% filters$phase & .data$trial_type %in% filters$trial_type))
  }
  parsed_experiment
}

