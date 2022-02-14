#' Get options for heidi model
#' @param iterations An integer denoting the number of iterations to run. Default = 1.
#' @param miniblocks A logical denoting whether to create trial miniblocks when possible. Default = TRUE.
#' @examples
#' get_heidi_opts(iterations = 10)
#' @export
get_heidi_opts <- function(iterations = 1, miniblocks = TRUE){
  return(list(iterations = iterations, miniblocks = miniblocks))
}
