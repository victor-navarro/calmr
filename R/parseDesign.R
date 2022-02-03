#' Parse each string in a data.frame
#' @param df A data.frame of dimensions Groups, 2*Phases+1
#'
#' @return A list of length nrow(df), the number of groups in the table.
#' @note
#' \itemize{
#' \item{
#' Each entry in even-numbered columns of df is a string formatted as per trialParser.
#' }
#' \item{
#' Each element of the returned list list is itself a list of length (ncol(df)-1)/2, the number of phases
#' }
#' }
#' @examples
#' df <- data.frame(Group = c('Group 1', 'Group 2'), P1 = c('10AB(US)', '10A(US)'), R1 = c(TRUE, TRUE))
#' parseDesign(df)
#' @seealso trialParser
#' @export
parseDesign <- function(df){
  design_list = vector('list', nrow(df))
  phases = colnames(df)
  groups = df[, 'Group']
  for (g in 1:nrow(df)){
    glist = vector('list', (ncol(df)-1)/2)
    for (p in seq(2, ncol(df), 2)){
      glist[[p/2]] = c(list(group = groups[g],
                            phase = phases[p],
                            parse_string = df[g, p],
                            randomize = df[g, p+1]),
                       trialParser(df[g, p]))
    }
    design_list[[g]] = glist
  }
  return(design_list)
}
