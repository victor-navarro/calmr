#' Parses a string stipulating trials
#'
#' @param str A string
#' @return A list with the number of trials, the trials, and the stimulus names
#' @examples
#' trial_parser("30XAU/30(YA)A")
#' @export
trial_parser <- function(str){
  ts = unlist(stringr::str_split(str, '/'))
  trial_names = sapply(ts, function(x) gsub("(\\d)+", "", x), USE.NAMES = F)
  trial_repeats = sapply(ts, function(x) as.numeric(stringr::str_extract(x, "(\\d)+")), USE.NAMES = F)
  trial_repeats[is.na(trial_repeats)] = 1
  complex_stims = sapply(trial_names, function(x) unlist(stringr::str_extract_all(x, "(?<=\\().+?(?=\\))")), USE.NAMES = F, simplify = F)
  simple_stims = stringr::str_split(gsub("\\([^()]*\\)", "", trial_names), "")

  #combine to make trials
  trial_list = sapply(1:length(trial_names), function(x) c(simple_stims[[x]], complex_stims[[x]]), simplify = F)

  return(list(trial_names = trial_names,
              trial_repeats = trial_repeats,
              trial_list = trial_list,
              stimuli = unique(unlist(c(simple_stims, complex_stims)))))
}
