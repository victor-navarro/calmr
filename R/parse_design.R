#' Parse each string in a design data.frame
#' @param df A data.frame of dimensions Groups, 2*Phases+1
#'
#' @return A tibble containing the parsed design in long format and the raw design.
#' @note
#' \itemize{
#' \item{
#' Each entry in even-numbered columns of df is a string formatted as per trial_parser.
#' }
#' }
#' @examples
#' df <- data.frame(Group = c("Group 1", "Group 2"), P1 = c("10AB(US)", "10A(US)"), R1 = c(TRUE, TRUE))
#' parse_design(df)
#' @seealso \code{\link{trial_parser}}
#' @export
parse_design <- function(df) {
  # if already parsed, skip
  if ("tbl" %in% class(df)) {
    return(df)
  }

  phases <- colnames(df)
  groups <- df[, 1]
  design <- tibble::tibble()
  for (g in 1:nrow(df)) {
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
  design
}
