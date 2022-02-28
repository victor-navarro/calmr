#' An assortment of functions to help heidi
#' @description
#' get_params generates a data.frame with stimulus salience parameters.
#' gen_ss_weights generates a named array with model weights.
#' parse_ws, parse_vs, parse_rs and parse_heidi_results parse the raw outputs of trainPavHEIDI into a readable format.
#' filter_heidi_results is a convenience function to filter specific phase and trial_type data.
#' @param design An experimental design. Either a data.frame or a tibble returned by parse_design
#' @param default_par A float between 0 and 1
#' @param stims A character vector with stimuli
#' @param default_val Default alpha value
#' @param mod A model list, as returned by trainPavHEIDI
#' @param raw_results A tibble with model information, as returned by run_heidi
#' @param parsed_results A list with parsed results, as returned by parse_heidi_results
#' @param filters A named list containing "phase" and "trial_type" character vectors, for filtering data
#' @import magrittr
#' @name heidi_helpers
NULL
#> NULL
#' @rdname heidi_helpers
#' @export
get_params <- function(design, default_par){
  if (!tibble::is_tibble(design)){
    design = parse_design(design)
  }
  stims = design %>%
    tidyr::unnest_wider(trial_info) %>%
    dplyr::select(stimuli) %>%
    unlist() %>%
    unique()
  data.frame(Stimulus = stims, Alpha = default_par)
}
#' @rdname heidi_helpers
#' @export
gen_ss_weights <- function(stims, default_val = 0){
  mat = matrix(default_val, ncol = length(stims), nrow = length(stims)) #perhaps a diagonal with 1s? Would accommodate self-association but increases model complexity.
  rownames(mat) = stims
  colnames(mat) = stims
  return(mat)
}
#' @rdname heidi_helpers
parse_ws <- function(mod){
  df = tibble::enframe(apply(mod$ws, 1, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::filter(Var1 != Var2) %>% dplyr::rename(s1 = Var1, s2 = Var2, value = Freq)
  return(df)
}
#' @rdname heidi_helpers
parse_vs <- function(mod){
  combvs = tibble::enframe(lapply(mod$combvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq) %>% dplyr::mutate(v_type = 'comb_v')
  chainvs = tibble::enframe(lapply(mod$chainvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq) %>% dplyr::mutate(v_type = 'chain_v')
  return(rbind(combvs, chainvs))
}

#' @rdname heidi_helpers
parse_rs <- function(mod){
  rs = tibble::enframe(lapply(mod$rs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq)
  return(rs)
}
#' @rdname heidi_helpers
parse_as <- function(mod){
  as = as.data.frame(mod$as) %>%
    dplyr::mutate(trial = 1:dplyr::n()) %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::pivot_longer(cols = -c("trial", "trial_type", "phase", "block_size"), names_to = "s1")
}
#' @rdname heidi_helpers
#' @export
parse_heidi_results <- function(raw_results){
  #expects a tibble with one row per group
  #returns a list with all the relevant data for exporting (and plotting)
  full_results = raw_results %>%
    dplyr::mutate(ws = list(parse_ws(mod_data)), vs = list(parse_vs(mod_data)),
                  rs = list(parse_rs(mod_data)), as = list(parse_as(mod_data))) %>%
    dplyr::ungroup()
  return(list(ws = full_results %>% dplyr::select(group, ws) %>% tidyr::unnest(ws) %>%
                dplyr::group_by(group, trial, trial_type, phase, s1, s2, block_size) %>% #summarize
                dplyr::summarise(value = mean(value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(group), s1 = as.factor(s1), s2 = as.factor(s2), trial_type = as.factor(trial_type), phase = as.factor(phase)),
              vs = full_results %>% dplyr::select(group, vs) %>% tidyr::unnest(vs) %>%
                dplyr::group_by(group, trial, phase, trial_type, v_type, s1, s2, block_size) %>%
                dplyr::summarise(value = mean(value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(group), trial_type = as.factor(trial_type), v_type = as.factor(v_type), s1 = as.factor(s1), s2 = as.factor(s2), phase = as.factor(phase)),
              rs = full_results %>% dplyr::select(group, rs) %>% tidyr::unnest(rs) %>%
                dplyr::group_by(group, trial, phase, trial_type, s1, s2, block_size) %>% #summarize
                dplyr::summarise(value = mean(value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(group), trial_type = as.factor(trial_type), s1 = as.factor(s1), s2 = as.factor(s2), phase = as.factor(phase)),
              as = full_results %>% dplyr::select(group, as) %>% tidyr::unnest(as) %>%
                dplyr::group_by(group, trial, phase, trial_type, s1, block_size) %>% #summarize
                dplyr::summarise(value = mean(value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(group), trial_type = as.factor(trial_type), s1 = as.factor(s1), phase = as.factor(phase))
  )
  )
}

#' @rdname heidi_helpers
#' @export
filter_heidi_results <- function(parsed_results, filters){
  lapply(parsed_results, function(x) x %>% dplyr::filter(phase %in% filters$phase & trial_type %in% filters$trial_type))
}

#' @rdname heidi_helpers
#' @export
get_similarity_mat <- function(param_df){
  m = matrix(FALSE,
             nrow = nrow(param_df),
             ncol = nrow(param_df),
             dimnames = list(param_df$Stimulus, param_df$Stimulus))
  diag(m) = TRUE
  #Could add a smart guess over here
  m
}



