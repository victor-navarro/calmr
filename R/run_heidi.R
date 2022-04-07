#' @title Run the HeiDI model
#' @description Runs the model under especific arguments.
#' @param args A tibble as returned by make_heidi_args.
#' @param parse A logical specifying whether the results should be parsed. Default = TRUE.
#' @return A list with parsed results or a tibble with raw results
#' @seealso get_heidi_opts, parse_design, train_pav_heidi, quick_heidi, parse_heidi_results
#' @export
run_heidi <- function(args, parse = TRUE){
  results = args %>%
    dplyr::rowwise() %>%
    dplyr::mutate(mod_data = list(train_pav_heidi(sals = .data$stim_alphas,
                                                  V = gen_ss_weights(.data$unique_functional_stimuli),
                                                  tps = .data$tps,
                                                  trial_pre_func = .data$trial_pre_func,
                                                  trial_post_func = .data$trial_post_func,
                                                  trial_pre_nomi = .data$trial_pre_nomi,
                                                  trial_post_nomi = .data$trial_post_nomi,
                                                  nomi_func_map = .data$nomi_func_map,
                                                  trial_names = .data$trial_names,
                                                  phase = .data$phase,
                                                  block_size = .data$block_size,
                                                  is_test = .data$is_test)))
  if (parse){
    results = parse_heidi_results(results)
  }
  results
}
