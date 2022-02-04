#' Returns a data.frame with parameters for stimuli contained in design
#'
#' @param design A tibble with the experimental design, as returned by parse_design
#' @param default_par A float between 0 and 1.
#' @return A data.frame with columns Stimulus and Alpha, for the stimulus name and its salience, respectively.
#' @seealso parse_design
#' @export

get_params <- function(design, default_par){
  stims = design %>%
    tidyr::unnest_wider(trial_info) %>%
    dplyr::select(stimuli) %>%
    unlist() %>%
    unique()
  data.frame(Stimulus = stims, Alpha = default_par)
}
