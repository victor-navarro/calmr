#' Parses a string stipulating trials
#'
#' @param trial_string A string
#' @return A list with
#' \itemize{
#' \item {trial_names: A character vector specifying the names of the trials contained in the string}
#' \item {trial_repeats: A numeric vector specifying the number of times each trial is given}
#' \item {trial_pre_functional: A list of character vectors specifying the unique functional stimuli on the expectation part of each trial}
#' \item {trial_post_functional: A list of character vectors specifying the unique functional stimuli on the correction part of each trial}
#' \item {trial_pre_nominal: A list of character vectors specifying the nominal stimuli on the expectation part of each trial}
#' \item {trial_post_nominal: A list of character vectors specifying the nominal stimuli on the expectation part of each trial}
#' \item {nomi_func_map: A data.frame specifying the mapping between functional and nominal stimuli}
#' \item {unique_nominal stimuli: A character vector with the unique nominal stimuli}
#' \item {unique_functional_stimuli: A character vector with the unique functional stimuli}
#' \item {is_test: A logical vector specifying whether each trial should not result in learning}
#' \item
#' }
#' @examples
#' trial_parser("30A")
#' trial_parser("30XA>U/30(YA)>A")
#' trial_parser("1X#/2X>(US)#")
#' trial_parser("1X>(US_a)/1Y>(US_b)")
#' @note
#' Supports complex stimuli in parentheses (e.g., "5A>(US)")
#' Supports multiple nominal versions of stimuli (e.g., "3(A_a)>(US_a)/3(A_b)>(US_b)")
#' Supports probe trials followed by a hash (e.g., "3X#")
#' On trials that do not have the ">" character, it is assumed that all the specified stimuli are to be used in the expectation step. If so, the "post" entries in the list will contain NAs, as necessary.
#' On those trials, the "post" stimuli will be none (a character(0) vector).
#' @export
trial_parser <- function(trial_string){
  #return list with nulls if trial_string is empty
  if (!nchar(trial_string)){
  return(list(trial_names = NULL,
       trial_repeats = 0,
       trial_pre_functional = NULL,
       trial_post_functional = NULL,
       trial_pre_nominal = NULL,
       trial_post_nominal = NULL,
       nomi_func_map = NULL,
       unique_nominal_stimuli = NULL,
       unique_functional_stimuli = NULL,
       is_test = NULL))
  }

  ts = unlist(stringr::str_split(trial_string, '/'))
  trial_names = sapply(ts, function(x) gsub("(\\d)+", "", x), USE.NAMES = F)
  trial_repeats = sapply(ts, function(x) as.numeric(stringr::str_extract(x, "(\\d)+")), USE.NAMES = F)
  trial_repeats[is.na(trial_repeats)] = 1

  #check if we have tests
  is_test = unlist(lapply(trial_names, function(x) stringr::str_detect(x, "#")))


  #get the pre nominal stimuli
  pre_nomi = sapply(sub("#", "", trial_names),
                    function(x) ifelse(stringr::str_detect(x, ">"),
                                       stringr::str_extract(x, "[^>]+"),
                                       x))
  complex_pre_nomi = sapply(pre_nomi, function(x) unlist(stringr::str_extract_all(x, "(?<=\\().+?(?=\\))")), simplify = F)
  simple_pre_nomi = stringr::str_split(gsub("\\([^()]*\\)", "", pre_nomi), "")
  simple_pre_nomi = lapply(simple_pre_nomi, function(x) if (!length(x)) NA else x) #wrestling the str_split function into returning NAs instead of character(0)

  #get the post nominal stimuli
  post_nomi = sapply(sub("#", "", trial_names),
                     function(x) ifelse(stringr::str_detect(x, ">"),
                                        sub(".*>", "", x),
                                        NA))
  complex_post_nomi = sapply(post_nomi, function(x) unlist(stringr::str_extract_all(x, "(?<=\\().+?(?=\\))")),
                             simplify = F)
  simple_post_nomi = stringr::str_split(gsub("\\([^()]*\\)", "", post_nomi), "")
  simple_post_nomi = lapply(simple_post_nomi, function(x) if (!length(x)) NA else x) #wrestling the str_split function into returning NAs instead of character(0)

  #combine simple and complex stimuli to make pre and post nominal stimuli
  trial_pre_nominal = sapply(1:length(trial_names), function(x) c(simple_pre_nomi[[x]], complex_pre_nomi[[x]]), simplify = F)
  trial_post_nominal = sapply(1:length(trial_names), function(x) c(simple_post_nomi[[x]], complex_post_nomi[[x]]), simplify = F)

  #save nominal stimuli before further processing
  unique_nominal_stimuli = unique(unlist(c(simple_pre_nomi, simple_post_nomi,
                                           complex_pre_nomi, complex_post_nomi)))
  unique_nominal_stimuli = unique_nominal_stimuli[!is.na(unique_nominal_stimuli)]

  #Now deal with functional
  #remove suffixes to make functional stimuli
  simple_pre_func = lapply(simple_pre_nomi, function(x) gsub("_.*", "", x))
  simple_post_func = lapply(simple_post_nomi, function(x) gsub("_.*", "", x))
  complex_pre_func = lapply(complex_pre_nomi, function(x) gsub("_.*", "", x))
  complex_post_func = lapply(complex_post_nomi, function(x) gsub("_.*", "", x))

  #now bundle simple and complex stimuli into pre and post functional stimuli
  trial_pre_functional = sapply(1:length(trial_names), function(x) c(simple_pre_func[[x]], complex_pre_func[[x]]), simplify = F)
  trial_post_functional = sapply(1:length(trial_names), function(x) c(simple_post_func[[x]], complex_post_func[[x]]), simplify = F)

  #save functional stimuli
  unique_functional_stimuli = unique(unlist(c(trial_pre_functional, trial_post_functional)))
  unique_functional_stimuli = unique_functional_stimuli[!is.na(unique_functional_stimuli)]

  #create mapping between nominal and functional stimuli
  nomi2func = stats::setNames(unlist(c(trial_pre_functional, trial_post_functional)),
                                  unlist(c(trial_pre_nominal, trial_post_nominal)))
  nomi2func = nomi2func[!duplicated(names(nomi2func)) & !is.na(nomi2func)]
  func2nomi = stats::setNames(names(nomi2func), nomi2func)

  #now we can clean everything of NAs
  trial_pre_functional = lapply(trial_pre_functional, function(x) x[!is.na(x)])
  trial_post_functional = lapply(trial_post_functional, function(x) x[!is.na(x)])
  trial_pre_nominal = lapply(trial_pre_nominal, function(x) x[!is.na(x)])
  trial_post_nominal = lapply(trial_post_nominal, function(x) x[!is.na(x)])

  return(list(trial_names = trial_names,
              trial_repeats = trial_repeats,
              trial_pre_functional = trial_pre_functional,
              trial_post_functional = trial_post_functional,
              trial_pre_nominal = trial_pre_nominal,
              trial_post_nominal = trial_post_nominal,
              nomi2func = nomi2func,
              func2nomi = func2nomi,
              unique_nominal_stimuli = unique_nominal_stimuli,
              unique_functional_stimuli = unique_functional_stimuli,
              is_test = is_test))
}
