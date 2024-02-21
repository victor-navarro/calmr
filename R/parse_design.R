#' Parse design data.frame
#' @param df A data.frame of dimensions Groups, 2*Phases+1
#' @param model An optional model to augment the design. See ??augment design
#' @param ... Other arguments passed to augment_design
#' @return A CalmrDesign object
#' @note
#' \itemize{
#' \item{
#' Each entry in even-numbered columns of df is
#' a string formatted as per trial_parser.
#' }
#' }
#' @examples
#' df <- data.frame(
#'   Group = c("Group 1", "Group 2"),
#'   P1 = c("10AB(US)", "10A(US)"), R1 = c(TRUE, TRUE)
#' )
#' parse_design(df)
#' @seealso \code{\link{trial_parser}}
#' @export

parse_design <- function(df, model = NULL, ...) {
  # if already parsed, skip
  if ("CalmrDesign" %in% class(df)) {
    design_obj <- df
    # augment if it hasn't been augmented
    if (!is.null(model) && !df@augmented) {
      design_obj <- augment_design(design_obj, ...)
    }
  } else {
    phases <- colnames(df)
    groups <- df[, 1]
    design <- tibble::tibble()
    for (g in seq_len(nrow(df))) {
      for (p in seq(2, ncol(df), 2)) {
        design <- rbind(design, tibble::tibble(
          group = groups[g],
          phase = phases[p],
          parse_string = df[g, p],
          randomize = df[g, p + 1],
          phase_info = list(phase_parser(df[g, p]))
        ))
      }
    }
    # That's the easy part
    # The hard part is to create the mapping for the experiment
    map <- .get_mapping(design)

    # create design object
    design_obj <- methods::new("CalmrDesign",
      design = design, mapping = map, raw_design = df
    )
    # augment design if required
    if (!is.null(model)) {
      design_obj <- augment_design(design_obj, model = model)
    }
  }
  design_obj
}

.get_mapping <- function(design) {
  # trial names
  tinfo <- lapply(design$phase_info, "[[", "trial_info")
  ginfo <- lapply(design$phase_info, "[[", "general_info")

  # gather
  mastert <- unlist(lapply(ginfo, "[[", "trial_names"))
  funcs <- unlist(lapply(tinfo, function(r) {
    lapply(r, "[[", "functionals")
  }), recursive = FALSE)
  nomis <- unlist(lapply(tinfo, function(r) {
    lapply(r, "[[", "nominals")
  }), recursive = FALSE)
  # reduce
  uniqs <- !duplicated(mastert)
  tnames <- mastert[uniqs]
  funcs <- setNames(funcs[uniqs], tnames)
  nomis <- setNames(nomis[uniqs], tnames)
  uni_fun <- unique(unlist(funcs))
  uni_nom <- unique(unlist(nomis))

  # make stimulus mapping
  n2f <- unlist(lapply(ginfo, "[[", "nomi2func"))
  n2f <- n2f[!duplicated(names(n2f))]
  f2n <- unlist(lapply(ginfo, "[[", "func2nomi"))
  f2n <- f2n[!duplicated(names(f2n))]

  list(
    trial_names = tnames,
    unique_functional_stimuli = uni_fun,
    unique_nominal_stimuli = uni_nom,
    trial_functionals = funcs,
    trial_nominals = nomis,
    nomi2func = n2f,
    func2nomi = f2n
  )
}
