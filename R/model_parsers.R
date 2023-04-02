#' Model parsers
#' @description An assorment of functions to help models

.parse <- function(mod, type){
  #check if mod has the type
  if (!(type %in% names(mod@model_results))) stop(sprintf("Model does not contain '%s' in model results.", type))
  dat = NULL
  if (type == "es"){
    dat = tibble::enframe(apply(mod@model_results$es, 1, function(x) as.data.frame(as.table(x))), name = "trial") %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest("value") %>%
      dplyr::mutate(Var1 = as.character(.data$Var1),
                    Var2 = as.character(.data$Var2)) %>%
      dplyr::filter(.data$Var1 != .data$Var2) %>%
      dplyr::rename("trial_type" = "tp", "s1" = "Var1", "s2" = "Var2", "value" = "Freq")
  }
  if (type %in% c("vs", "evs", "ivs")){
    dat = tibble::enframe(apply(mod@model_results[[type]], 1, function(x) as.data.frame(as.table(x))), name = "trial") %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest("value") %>%
      dplyr::mutate(Var1 = as.character(.data$Var1),
                    Var2 = as.character(.data$Var2)) %>%
      dplyr::filter(.data$Var1 != .data$Var2) %>%
      dplyr::rename("trial_type" = "tp", "s1" = "Var1", "s2" = "Var2", "value" = "Freq")
  }
  if (type == "acts"){
    combs = tibble::enframe(lapply(mod@model_results$acts$combvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest("value") %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                                   Var2 = as.character(.data$Var2)) %>%
      dplyr::rename("trial_type" = "tp", "s1" = "Var1", "s2" = "Var2", "value" = "Freq") %>% dplyr::mutate(act_type = 'comb')

    chains = tibble::enframe(lapply(mod@model_results$acts$chainvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest("value") %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                                   Var2 = as.character(.data$Var2)) %>%
      dplyr::rename("trial_type" = "tp", "s1" = "Var1", "s2" = "Var2", "value" = "Freq") %>%
      dplyr::mutate(act_type = 'chain')
    dat = rbind(combs, chains)
  }
  if (type == "as"){
    dat = as.data.frame(mod@model_results$as) %>%
      dplyr::mutate(trial = 1:dplyr::n()) %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::pivot_longer(cols = -c("trial", "group", "is_test", "tp",
                                    "phase", "block_size"), names_to = "s1") %>%
      dplyr::rename("trial_type" = "tp")
  }
  if (type == "rs"){
    dat = tibble::enframe(apply(mod@model_results$rs, 1, function(x) as.data.frame(as.table(x))), name = "trial") %>%
      dplyr::bind_cols(mod@experience) %>%
      dplyr::mutate(tp = mod@mapping$trial_names[mod@experience$tp]) %>%
      tidyr::unnest("value") %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                                   Var2 = as.character(.data$Var2)) %>%
      dplyr::rename("trial_type" = "tp", "s1" = "Var1", "s2" = "Var2", "value" = "Freq")
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
  #expects a CalmrExperiment
  #puts in it a list with all the relevant data for exporting (and plotting)
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
  agg = list()
  vars = names(parsed_experiment@results$parsed_mod_responses[[1]])
  dat = parsed_experiment@results %>%
    tidyr::unnest_wider("parsed_mod_responses") %>%
    dplyr::ungroup()
  #check for Konorskian models
  if (all(c("evs", "ivs") %in% vars)){
    vars = vars[!(vars %in% c("evs", "ivs"))]
    vars = c(vars, "eivs")
  }
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
  if (type == "vs"){
    dat = do.call("rbind", res[[type]]) %>%
      dplyr::group_by(.data$group, .data$trial, .data$trial_type, .data$phase,
                      .data$s1, .data$s2, .data$block_size) %>% #summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(group = as.factor(.data$group), s1 = as.factor(.data$s1),
                    s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
                    phase = as.factor(.data$phase))
  }
  if (type == "eivs"){
    ev = do.call("rbind", res$evs) %>%
      dplyr::group_by(.data$group, .data$trial, .data$trial_type, .data$phase,
                      .data$s1, .data$s2, .data$block_size) %>% #summarize
      dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(group = as.factor(.data$group), s1 = as.factor(.data$s1),
                    s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
                    phase = as.factor(.data$phase), assoc_type = "Excitatory")
    iv = do.call("rbind", res$ivs) %>%
      dplyr::group_by(.data$group, .data$trial, .data$trial_type, .data$phase,
                      .data$s1, .data$s2, .data$block_size) %>% #summarize
      dplyr::summarise(value = -mean(.data$value), .groups = "drop") %>%
      dplyr::mutate(group = as.factor(.data$group), s1 = as.factor(.data$s1),
                    s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
                    phase = as.factor(.data$phase), assoc_type = "Inhibitory")
    net = ev
    net$value = net$value+iv$value
    net$assoc_type = "Net"
    dat = rbind(ev, iv, net)
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

