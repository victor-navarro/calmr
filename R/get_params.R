#' Returns a data.frame with parameters for stimuli contained in design
#'
#' @param design_list A list with the experimental design, as returned by parse_design
#' @param default_par A float between 0 and 1.
#' @return A data.frame with columns Stimulus and Alpha, for the stimulus name and its salience, respectively.
#' @seealso parse_design
#' @export

get_params <- function(design_list, default_par){
  #
  stims = sort(unique(unlist(lapply(design_list, function(g) lapply(g, function(p) p$stimuli)))))
  return(data.frame(Stimulus = stims, Alpha = default_par))
}
