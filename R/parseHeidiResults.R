#' Parse heidi results
#' @param raw_results A tibble containing a mod_data column, as returned by runHeidi
#' @returns A list containing parsed ws, vs, and rs.
#' @seealso runHeidi
#' @export
#' @importFrom magrittr %>%
parseHeidiResults <- function(raw_results){
  #expects a tibble with one row per group
  #returns a list with all the relevant data for exporting (and plotting)
  full_results = raw_results %>% dplyr::mutate(ws = list(parseWs(mod_data)), vs = list(parseVs(mod_data)), rs = list(parseRs(mod_data)))
  return(list(ws = full_results %>% dplyr::select(iteration, group, ws) %>% tidyr::unnest(ws) %>%
                dplyr::mutate(group = as.factor(group), s1 = as.factor(s1), s2 = as.factor(s2), trial_type = as.factor(trial_type)),
              vs = full_results %>% dplyr::select(iteration, group, vs) %>% tidyr::unnest(vs) %>%
                dplyr::mutate(group = as.factor(group), trial_type = as.factor(trial_type), s1 = as.factor(s1), s2 = as.factor(s2)),
              rs = full_results %>% dplyr::select(iteration, group, rs) %>% tidyr::unnest(rs) %>%
                dplyr::mutate(group = as.factor(group), trial_type = as.factor(trial_type), s1 = as.factor(s1), s2 = as.factor(s2))))
}
