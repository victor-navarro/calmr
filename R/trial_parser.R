#' Parses a string stipulating trials
#'
#' @param str A string
#' @return A list with the number of trials, the trials, and the stimulus names
#' @examples
#' trial_parser("30XAU/30(YA)A")
#' trial_parser("1X#/2X(US)#")
#' trial_parser("1X(US_a)/1Y(US_b)")
#' @note
#' Supports complex stimuli in parentheses (e.g., "5A(US)")
#' Supports multiple versions of stimuli (e.g., "3(A_a)(US_a)/3(A_b)(US_b)")
#' Supports probe trials, followed by a hash (e.g., "3X#")
#' @export
trial_parser <- function(str){
  ts = unlist(stringr::str_split(str, '/'))
  trial_names = sapply(ts, function(x) gsub("(\\d)+", "", x), USE.NAMES = F)
  trial_repeats = sapply(ts, function(x) as.numeric(stringr::str_extract(x, "(\\d)+")), USE.NAMES = F)
  trial_repeats[is.na(trial_repeats)] = 1
  complex_stims = sapply(trial_names, function(x) unlist(stringr::str_extract_all(x, "(?<=\\().+?(?=\\))")), USE.NAMES = F, simplify = F)
  simple_stims = stringr::str_split(gsub("\\([^()]*\\)", "", trial_names), "")

  #check if we have tests
  is_test = lapply(simple_stims, function(x) any("#" %in% x))
  simple_stims = lapply(simple_stims, function(x) x[x!="#"])

  #combine to make nominal stimuli
  trial_stimuli_nominal = sapply(1:length(trial_names), function(x) c(simple_stims[[x]], complex_stims[[x]]), simplify = F)

  #save nominal stimuli before further processing
  unique_nominal_stimuli = unique(unlist(c(simple_stims, complex_stims)))

  #remove suffixes to make stimuli nodes
  complex_stims = lapply(complex_stims, function(x) gsub("_.*", "", x))
  trial_stimuli_functional = sapply(1:length(trial_names), function(x) c(simple_stims[[x]], complex_stims[[x]]), simplify = F)

  #save functional stimuli
  unique_functional_stimuli = unique(unlist(c(simple_stims, complex_stims)))

  nomi_func_map = unique(data.frame(nomi = unlist(trial_stimuli_nominal), func = unlist(trial_stimuli_functional)))

  return(list(trial_names = trial_names,
              trial_repeats = trial_repeats,
              trial_stimuli_functional = trial_stimuli_functional,
              trial_stimuli_nominal = trial_stimuli_nominal,
              nomi_func_map = nomi_func_map,
              unique_nominal_stimuli = unique_nominal_stimuli,
              unique_functional_stimuli = unique_functional_stimuli,
              is_test = is_test))
}
