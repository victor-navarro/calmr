#' Parse design data.frame
#' @param df A data.frame of dimensions Groups, 2*Phases+1
#'
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

parse_design <- function(df) {
  # if already parsed, skip
  if ("CalmrDesign" %in% class(df)) {
    return(df)
  } else {
    design <- tryCatch(
      {
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
              trial_info = list(trial_parser(df[g, p]))
            ))
          }
        }
        # That's the easy part

        # The hard part is to create the mapping for the experiment
        tinfo <- tidyr::unnest_wider(design, "trial_info")
        tinfo <- tinfo[, c(
          "trial_pre_functional",
          "trial_post_functional",
          "trial_pre_nominal",
          "trial_post_nominal",
          "trial_names",
          "nomi2func",
          "func2nomi"
        )]

        # gather
        tpref <- unlist(tinfo$trial_pre_functional, recursive = FALSE)
        tpostf <- unlist(tinfo$trial_post_functional, recursive = FALSE)
        tpren <- unlist(tinfo$trial_pre_nominal, recursive = FALSE)
        tpostn <- unlist(tinfo$trial_post_nominal, recursive = FALSE)
        mastert <- unlist(tinfo$trial_names)

        # reduce
        tnames <- mastert[!duplicated(mastert)]
        tpref <- setNames(tpref[!duplicated(mastert)], tnames)
        tpostf <- setNames(tpostf[!duplicated(mastert)], tnames)
        tpren <- setNames(tpren[!duplicated(mastert)], tnames)
        tpostn <- setNames(tpostn[!duplicated(mastert)], tnames)
        mastert <- mastert[!duplicated(mastert)]

        # make stimulus mapping
        uni_fun <- unique(unlist(c(tpref, tpostf)))
        uni_nom <- unique(unlist(c(tpren, tpostn)))
        n2f <- unlist(tinfo$nomi2func)
        n2f <- n2f[!duplicated(names(n2f))]
        f2n <- unlist(tinfo$func2nomi)
        f2n <- f2n[!duplicated(names(f2n))]

        map <- list(
          trial_names = mastert,
          unique_functional_stimuli = uni_fun,
          unique_nominal_stimuli = uni_nom,
          trial_pre_func = tpref,
          trial_post_func = tpostf,
          trial_pre_nomi = tpren,
          trial_post_nomi = tpostn,
          nomi2func = n2f,
          func2nomi = f2n
        )

        # done
        return(methods::new("CalmrDesign",
          design = design, map = map, raw_design = df
        ))
      },
      error = function(e) {
        stop("Could not parse design. Please see examples in ??parse_design")
      }
    )
  }
}
