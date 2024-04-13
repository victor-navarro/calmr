#' Parses a phase string
#'
#' @param phase_string A string specifying trials within a phase.
#' @return A named list with:
#' \describe{
#' \item{trial_info:}{A trial-named list of lists.}
#' \item{general_info:}{General phase information.}
#' }
#' @note This function is meant for internal use only,
#' but we expose it so you can test your strings.
#' @examples
#' # A silly (but valid) string
#' phase_parser("10#Rescorla>Wagner")
#'
#' # An invalid string that needs trial repetitions for one of trials.
#' try(phase_parser("10#Rescorla/Wagner"))
#' @seealso [parse_design()]
#' @export
phase_parser <- function(phase_string) {
  # check for empty phase_strings
  if (!nchar(phase_string)) {
    return(NULL)
  }

  randomize <- grepl("!", phase_string)
  phase_string <- gsub("!", "", phase_string)

  ts <- unlist(base::strsplit(phase_string, "/"))
  # parse each trial separately
  tinfo <- sapply(ts, .parse_trial, simplify = FALSE)
  # now prepare the general information
  # nomi2fun and func2nomi maps
  allnomi <- unname(unlist(lapply(tinfo, "[[", "all_nominals")))
  allfunc <- unname(unlist(lapply(tinfo, "[[", "all_functionals")))
  nomi2func <- stats::setNames(allfunc, allnomi)
  nomi2func <- nomi2func[!duplicated(names(nomi2func))]
  func2nomi <- stats::setNames(names(nomi2func), nomi2func)

  # unique nominal
  ginfo <- list(
    trial_names = unname(unlist(
      lapply(tinfo, "[[", "name")
    )),
    trial_repeats = unname(unlist(
      lapply(tinfo, "[[", "repetitions")
    )),
    is_test = unname(unlist(
      lapply(tinfo, "[[", "is_test")
    )),
    randomize = randomize,
    nomi2func = nomi2func,
    func2nomi = func2nomi
  )

  list(
    trial_info = tinfo,
    general_info = ginfo
  )
}
# takes a trial string
.parse_trial <- function(ts) {
  # remove repeats
  tn <- gsub("^\\d+", "", ts)
  # get repetitions
  treps <- as.numeric(regmatches(ts, regexpr("^\\d+", ts)))
  treps <- if (is.na(treps)) 1 else treps
  # detect test character
  is_test <- grepl("#", tn)
  # split into nominal periods (removing # from trial names)
  nomi_split <- unlist(strsplit(sub("#", "", tn), ">"))
  # extract complex stimuli
  nomi_complex <- sapply(
    nomi_split,
    function(x) {
      unlist(regmatches(x, gregexpr("(?<=\\().+?(?=\\))", x, perl = TRUE)))
    },
    simplify = FALSE
  )

  # extract simple stimuli
  nomi_simple <- sapply(
    nomi_split,
    function(x) {
      unlist(strsplit(gsub("\\([^()]*\\)", "", x), ""))
    },
    simplify = FALSE
  )
  # combine
  nomis <- sapply(nomi_split, function(p) {
    c(
      nomi_simple[[p]],
      nomi_complex[[p]]
    )
  }, simplify = FALSE)
  # get functionals
  funcs <- lapply(nomis, function(x) gsub("_.*", "", x))

  list(
    name = tn,
    repetitions = treps,
    is_test = is_test,
    periods = nomi_split,
    nominals = nomis,
    functionals = funcs,
    all_nominals = unlist(nomis, use.names = FALSE),
    all_functionals = unlist(funcs, use.names = FALSE)
  )
}
