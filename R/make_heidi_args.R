#' Make a tibble to fit a heidi model
#'
#' @param design A design list, as returned by parse_design
#' @param pars A parameter data.frame as returned by get_params
#' @param opts A list with options
#'
#' @return A tibble with the arguments required to run the model. Each row represents a group in the experimental design.
#'
#' @seealso parse_design, get_params
#' @examples
#' df <- data.frame(Group = c('Group 1', 'Group 2'), P1 = c('10AB(US)', '10A(US)'), R1 = c(TRUE, TRUE))
#' des <- parse_design(df)
#' ps <- get_params(des, 0.2)
#' make_heidi_args(design = des, pars = ps, opts = get_heidi_opts(iterations = 1))
#'
#' @export

make_heidi_args <- function(design, pars, opts){
  #Returns a tibble to run design in a rowwise manner (each row is a group)
  #Some early info
  snames = pars$Stimulus
  alphas = pars$Alpha
  names(alphas) = snames

  #the only challenge here is to create a master list of trials (trials)
  #and sample the training for each group (tps)
  #create master lists of trials and trial_names
  tinfo = design %>% tidyr::unnest_wider(.data$trial_info) %>%
    dplyr::select(.data$trial_pre_functional,
                  .data$trial_post_functional,
                  .data$trial_pre_nominal,
                  .data$trial_post_nominal,
                  .data$trial_names)

  if (class(tinfo$trial_names) == "list"){
    master_trial_names = do.call('c', tinfo$trial_names)
  }else{
    master_trial_names = tinfo$trial_names
  }
  trial_pre_functional_list = do.call('c', tinfo$trial_pre_functional)
  trial_post_functional_list = do.call('c', tinfo$trial_post_functional)
  trial_pre_nominal_list = do.call('c', tinfo$trial_pre_nominal)
  trial_post_nominal_list = do.call('c', tinfo$trial_post_nominal)

  #reduce
  trial_pre_functional_list = trial_pre_functional_list[!duplicated(master_trial_names)]
  trial_post_functional_list = trial_post_functional_list[!duplicated(master_trial_names)]

  trial_pre_nominal_list = trial_pre_nominal_list[!duplicated(master_trial_names)]
  trial_post_nominal_list = trial_post_nominal_list[!duplicated(master_trial_names)]

  master_trial_names = master_trial_names[!duplicated(master_trial_names)]

  #get some stimulus data and parameters
  sdata = design %>% dplyr::group_by(.data$group) %>%
    tidyr::unnest_wider(.data$trial_info) %>%
    #this bit saves us having to crunch data about stimuli that are not present in group but are present in the other
    dplyr::group_by(.data$group) %>%
    dplyr::mutate(unique_nominal_stimuli = list(unique(unlist(.data$unique_nominal_stimuli))),
                  unique_functional_stimuli = list(unique(unlist(.data$unique_functional_stimuli))),
                  nomi_func_map = list(unique(do.call('rbind', .data$nomi_func_map)))) %>%
    #this bit puts the required alphas on a row by row basis
    dplyr::rowwise() %>%
    dplyr::mutate(stim_alphas = list(alphas[unlist(.data$unique_nominal_stimuli)])) %>%
    dplyr::select(.data$group, .data$stim_alphas, .data$unique_functional_stimuli,
                  .data$unique_nominal_stimuli, .data$nomi_func_map) %>%
    dplyr::distinct()
  #we can sample now
  #Dom, if you are reading this, I apologize for this bit
  #It basically creates a tibble of iterations*groups*phases, with pointers for trials in the masterlist

  tb = design %>%
    tidyr::unnest_wider(.data$trial_info) %>%
    tidyr::expand_grid(iteration = 1:opts$iterations) %>%
    dplyr::rowwise() %>%
    #the pointers are returned by the function .sample_trial
    dplyr::mutate(samp = list(.sample_trials(.data$trial_names, .data$is_test, .data$trial_repeats,
                                             .data$randomize, opts$miniblocks, master_trial_names))) %>%
    dplyr::select(-.data$is_test) %>% #unfortunate naming
    tidyr::unnest_wider(.data$samp) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(phaselab = list(rep(.data$phase, length(.data$tps))),
                  blocks = list(rep(.data$block_size, length(.data$tps)))) %>%
    #one last manipulation to concatenate phases into single rows
    dplyr::group_by(.data$iteration, .data$group) %>%
    dplyr::summarize(tps = list(unlist(.data$tps)),
                     is_test = list(unlist(.data$is_test)),
                     phase = list(unlist(.data$phaselab)),
                     block_size = list(unlist(.data$blocks)))

  #now put the trial and stimulus information back
  tb = tb %>% dplyr::rowwise() %>%
    dplyr::mutate(trial_pre_func = list(trial_pre_functional_list),
                  trial_post_func = list(trial_post_functional_list),
                  trial_pre_nomi = list(trial_pre_nominal_list),
                  trial_post_nomi = list(trial_post_nominal_list),
                  trial_names = list(master_trial_names),
                  sdata[sdata$group == .data$group, ])
  tb
}

.sample_trials <- function(names, test, repeats, randomize, miniblocks, masterlist){
  #samples trials as a function of a masterlist, and randomizes if necessary
  tps = unlist(sapply(seq_along(names), function(n) rep(which(masterlist %in% names[n]), repeats[n])))
  tstps = unlist(sapply(seq_along(names), function(n) rep(test[n], repeats[n])))
  block_size = 1
  if (randomize){
    #create miniblocks, if requested
    if (length(repeats) > 1 & miniblocks){
      gcd = Reduce(.gcd, repeats)
      per_block = repeats/gcd
      block_size = sum(per_block)
      tps = c() #note the redefining
      tstps = c()
      for (b in 1:gcd){
        ts = unlist(sapply(seq_along(names), function(n) rep(which(masterlist %in% names[n]), per_block[n])))
        tsts = unlist(sapply(seq_along(names), function(n) rep(test[n], per_block[n])))
        #randomize
        ri = sample(length(ts))
        tps = c(tps, ts[ri])
        tstps = c(tstps, tsts[ri])
      }
    }else{
      #randomize
      ri = sample(length(tps))
      tps = tps[ri]
      tstps = tstps[ri]
    }
  }
  return(list(tps = tps, is_test = tstps, block_size = block_size))
}

#function to return the gcd
.gcd <-  function(x,y) {
  r <- x%%y;
  return(ifelse(r, .gcd(y, r), y))
}


