#' An assortment of functions to help heidi
#' @description genSSWeights generates a named array with model weights. parseWs, parseVs, and parseRs parse the raw outputs of trainPavHEIDI into a readable format.
#' @param stims A character vector with stimuli.
#' @param default_val Default alpha value.
#' @param mod A model list, as returned by trainPavHEIDI.
#' @importFrom magrittr %>%
#' @name heidi_helpers
NULL
#> NULL
#' @rdname heidi_helpers
#Function to initialize model weights
genSSWeights <- function(stims, default_val = 0){
  mat = matrix(default_val, ncol = length(stims), nrow = length(stims)) #perhaps a diagonal with 1s? Would accommodate self-association but increases model complexity.
  rownames(mat) = stims
  colnames(mat) = stims
  return(mat)
}
#' @rdname heidi_helpers
parseWs <- function(mod){
  #mod is the model
  tname = rownames(mod$ts)
  df = tibble::enframe(apply(mod$ws, 1, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trialnames[mod$ts]) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::filter(Var1 != Var2) %>% dplyr::rename(s1 = Var1, s2 = Var2, value = Freq)
  return(df)
}

#' @rdname heidi_helpers
parseVs <- function(mod){
  combvs = tibble::enframe(lapply(mod$combvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trialnames[mod$ts]) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq) %>% dplyr::mutate(v_type = 'comb_v')
  chainvs = tibble::enframe(lapply(mod$chainvs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trialnames[mod$ts]) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq) %>% dplyr::mutate(v_type = 'chain_v')
  return(rbind(combvs, chainvs))
}

#' @rdname heidi_helpers
parseRs <- function(mod){
  rs = tibble::enframe(lapply(mod$rs, function(x) as.data.frame(as.table(x))), name = 'trial') %>%
    dplyr::mutate(trial_type = mod$trialnames[mod$ts]) %>%
    tidyr::unnest(value) %>% dplyr::mutate(Var1 = as.character(Var1),
                                           Var2 = as.character(Var2)) %>%
    dplyr::rename(s1 = Var1, s2 = Var2, value = Freq)
  return(rs)
}
