#' An assortment of functions to help heidi
#' @description gen_ss_weights generates a named array with model weights. parse_ws, parse_vs, parse_rs and parse_heidi_results parse the raw outputs of trainPavHEIDI into a readable format.
#' @param stims A character vector with stimuli.
#' @param default_val Default alpha value.
#' @param mod A model list, as returned by trainPavHEIDI.
#' @param raw_results A tibble with model information, as returned by run_heidi
#' @param parsed_results A list with parsed results, as returned by parse_heidi_results
#' @param filters A named list containing "phase" and "trial_type" character vectors, for filtering data.
#' @import magrittr
#' @name heidi_helpers
NULL
#> NULL
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
    dplyr::mutate(trial_type = mod$trial_names[mod$ts], phase = mod$phase) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::filter(Var1 != Var2) %>% dplyr::rename(s1 = Var1, s2 = Var2, value = Freq)
  return(df)
}
#' @rdname heidi_helpers
parse_vs <- function(mod){
  combvs = tibble::enframe(lapply(mod$combvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$ts], phase = mod$phase) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq) %>% dplyr::mutate(v_type = 'comb_v')
  chainvs = tibble::enframe(lapply(mod$chainvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$ts], phase = mod$phase) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq) %>% dplyr::mutate(v_type = 'chain_v')
  return(rbind(combvs, chainvs))
}

#' @rdname heidi_helpers
parse_rs <- function(mod){
  rs = tibble::enframe(lapply(mod$rs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$ts], phase = mod$phase) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq)
  return(rs)
}
#' @rdname heidi_helpers
#' @export
parse_heidi_results <- function(raw_results){
  #expects a tibble with one row per group
  #returns a list with all the relevant data for exporting (and plotting)
  full_results = raw_results %>%
    dplyr::mutate(ws = list(parse_ws(mod_data)), vs = list(parse_vs(mod_data)), rs = list(parse_rs(mod_data))) %>%
    dplyr::ungroup()
  return(list(ws = full_results %>% dplyr::select(group, ws) %>% tidyr::unnest(ws) %>%
                dplyr::group_by(group, trial, trial_type, phase, s1, s2) %>% #summarize
                dplyr::summarise(value = mean(value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(group), s1 = as.factor(s1), s2 = as.factor(s2), trial_type = as.factor(trial_type), phase = as.factor(phase)),
              vs = full_results %>% dplyr::select(group, vs) %>% tidyr::unnest(vs) %>%
                dplyr::group_by(group, trial, phase, trial_type, v_type, s1, s2) %>%
                dplyr::summarise(value = mean(value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(group), trial_type = as.factor(trial_type), v_type = as.factor(v_type), s1 = as.factor(s1), s2 = as.factor(s2), phase = as.factor(phase)),
              rs = full_results %>% dplyr::select(group, rs) %>% tidyr::unnest(rs) %>%
                dplyr::group_by(group, trial, phase, trial_type, s1, s2) %>% #summarize
                dplyr::summarise(value = mean(value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(group), trial_type = as.factor(trial_type), s1 = as.factor(s1), s2 = as.factor(s2), phase = as.factor(phase))))
}

#' @rdname heidi_helpers
#' @export
filter_heidi_results <- function(parsed_results, filters){
  lapply(parsed_results, function(x) x %>% dplyr::filter(phase %in% filters$phase & trial_type %in% filters$trial_type))
}

