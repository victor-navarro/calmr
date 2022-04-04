#' An assortment of functions to help heidi
#' @description
#' get_params generates a data.frame with stimulus salience parameters.
#' gen_ss_weights generates a named array with model weights.
#' parse_ws, parse_vs, parse_rs and parse_heidi_results parse the raw outputs of train_pav_heidi into a readable format.
#' filter_heidi_results is a convenience function to filter specific phase and trial_type data.
#' @param design An experimental design. Either a data.frame or a tibble returned by parse_design
#' @param default_par A float between 0 and 1
#' @param stims A character vector with stimuli
#' @param default_val Default alpha value
#' @param mod A model list, as returned by train_pav_heidi
#' @param raw_results A tibble with model information, as returned by quick_heidi
#' @param parsed_results A list with parsed results, as returned by parse_heidi_results
#' @param filters A named list containing "phase" and "trial_type" character vectors, for filtering data
#' @import magrittr
#' @importFrom rlang .data
#' @name heidi_helpers
NULL
#> NULL
#' @rdname heidi_helpers
#' @export
get_params <- function(design, default_par = .2){
  if (!tibble::is_tibble(design)){
    design = parse_design(design)
  }
  stims = design %>%
    tidyr::unnest_wider(.data$trial_info) %>%
    dplyr::select(.data$unique_nominal_stimuli) %>%
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
  df = tibble::enframe(apply(mod$ws, 1, function(x) as.data.frame(as.table(x))), name = "trial") %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::unnest(.data$value) %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                           Var2 = as.character(.data$Var2)) %>%
    dplyr::filter(.data$Var1 != .data$Var2) %>%
    dplyr::rename(s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq)
  return(df)
}
#' @rdname heidi_helpers
parse_vs <- function(mod){
  combvs = tibble::enframe(lapply(mod$combvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::unnest(.data$value) %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                           Var2 = as.character(.data$Var2)) %>%
    dplyr::rename(s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq) %>% dplyr::mutate(v_type = 'comb_v')
  chainvs = tibble::enframe(lapply(mod$chainvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::unnest(.data$value) %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                           Var2 = as.character(.data$Var2)) %>%
    dplyr::rename(s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq) %>% dplyr::mutate(v_type = 'chain_v')
  return(rbind(combvs, chainvs))
}

#' @rdname heidi_helpers
parse_rs <- function(mod){
  rs = tibble::enframe(apply(mod$rs, 1, function(x) as.data.frame(as.table(x))), name = "trial") %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::unnest(.data$value) %>% dplyr::mutate(Var1 = as.character(.data$Var1),
                                           Var2 = as.character(.data$Var2)) %>%
    dplyr::rename(s1 = .data$Var1, s2 = .data$Var2, value = .data$Freq)
  return(rs)
}
#' @rdname heidi_helpers
parse_as <- function(mod){
  as = as.data.frame(mod$as) %>%
    dplyr::mutate(trial = 1:dplyr::n()) %>%
    dplyr::mutate(trial_type = mod$trial_names[mod$tps], phase = mod$phase, block_size = mod$block_size) %>%
    tidyr::pivot_longer(cols = -c(.data$trial, .data$trial_type,
                                  .data$phase, .data$block_size), names_to = "s1")
}
#' @rdname heidi_helpers
#' @export
parse_heidi_results <- function(raw_results){
  #expects a tibble with one row per group
  #returns a list with all the relevant data for exporting (and plotting)
  full_results = raw_results %>% dplyr::rowwise() %>%
    dplyr::mutate(ws = list(parse_ws(.data$mod_data)), vs = list(parse_vs(.data$mod_data)),
                  rs = list(parse_rs(.data$mod_data)), as = list(parse_as(.data$mod_data))) %>%
    dplyr::ungroup()
  return(list(ws = full_results %>% dplyr::select(.data$group, .data$ws) %>%
                tidyr::unnest(.data$ws) %>%
                dplyr::group_by(.data$group, .data$trial, .data$trial_type, .data$phase,
                                .data$s1, .data$s2, .data$block_size) %>% #summarize
                dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(.data$group), s1 = as.factor(.data$s1),
                              s2 = as.factor(.data$s2), trial_type = as.factor(.data$trial_type),
                              phase = as.factor(.data$phase)),
              vs = full_results %>% dplyr::select(.data$group, .data$vs) %>%
                tidyr::unnest(.data$vs) %>%
                dplyr::group_by(.data$group, .data$trial, .data$phase, .data$trial_type,
                                .data$v_type, .data$s1, .data$s2, .data$block_size) %>%
                dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
                              v_type = as.factor(.data$v_type), s1 = as.factor(.data$s1),
                              s2 = as.factor(.data$s2), phase = as.factor(.data$phase)),
              rs = full_results %>% dplyr::select(.data$group, .data$rs) %>%
                tidyr::unnest(.data$rs) %>%
                dplyr::group_by(.data$group, .data$trial, .data$phase, .data$trial_type,
                                .data$s1, .data$s2, .data$block_size) %>% #summarize
                dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
                              s1 = as.factor(.data$s1), s2 = as.factor(.data$s2),
                              phase = as.factor(.data$phase)),
              as = full_results %>% dplyr::select(.data$group, .data$as) %>%
                tidyr::unnest(.data$as) %>%
                dplyr::group_by(.data$group, .data$trial, .data$phase,
                                .data$trial_type, .data$s1, .data$block_size) %>% #summarize
                dplyr::summarise(value = mean(.data$value), .groups = "drop") %>%
                dplyr::mutate(group = as.factor(.data$group), trial_type = as.factor(.data$trial_type),
                              s1 = as.factor(.data$s1), phase = as.factor(.data$phase))
  )
  )
}

#' @rdname heidi_helpers
#' @export
filter_heidi_results <- function(parsed_results, filters){
  lapply(parsed_results, function(x) x %>% dplyr::filter(.data$phase %in% filters$phase & .data$trial_type %in% filters$trial_type))
}

