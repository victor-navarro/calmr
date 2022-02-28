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
  cons = rep(1, length(alphas))
  names(alphas) = names(cons) = snames

  #the only challenge here is to create a master list of trials (trials)
  #and sample the training for each group (tps)
  #create master lists of trials and trial_names
  tinfo = design %>% tidyr::unnest_wider(trial_info) %>% dplyr::select(trial_list, trial_names)
  if (class(tinfo$trial_names) == "list"){
    trial_names_masterlist = do.call('c', tinfo$trial_names)
  }else{
    trial_names_masterlist = tinfo$trial_names
  }
  trial_masterlist = do.call('c', tinfo$trial_list)
  #reduce
  trial_masterlist = trial_masterlist[!duplicated(trial_names_masterlist)]
  trial_names_masterlist = trial_names_masterlist[!duplicated(trial_names_masterlist)]

  #get some stimulus data and parameteres
  sdata = design %>% dplyr::group_by(group) %>%
    tidyr::unnest_wider(trial_info) %>%
    #this bit saves us having to crunch data about stimuli that are not present in group but are present in the other
    dplyr::group_by(group) %>%
    dplyr::mutate(stim_names = list(unique(unlist(stimuli)))) %>%
    #this bit puts the required alphas and constants, on a row by row basis
    dplyr::rowwise() %>%
    dplyr::mutate(stim_alphas = list(alphas[unlist(stim_names)]),
                  stim_cons = list(cons[unlist(stim_names)])) %>%
    dplyr::select(group, stim_names, stim_alphas, stim_cons) %>%
    dplyr::distinct()
  #we can sample now
  #Dom, if you are reading this, I apologize for this bit
  #It basically creates a tibble of iterations*groups*phases, with pointers for trials in the masterlist
  tb = design %>%
    tidyr::unnest_wider(trial_info) %>%
    tidyr::expand_grid(iteration = 1:opts$iterations) %>%
    dplyr::rowwise() %>%
    #the pointers are returned by the function .sample_trial
    dplyr::mutate(samp = list(.sample_trials(trial_names, is_test, trial_repeats, randomize, opts$miniblocks, trial_names_masterlist))) %>%
    tidyr::unnest_wider(samp) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(phaselab = list(rep(phase, length(tps))), blocks = list(rep(block_size, length(tps)))) %>%
    #one last manipulation to concatenate phases into single rows
    dplyr::group_by(iteration, group) %>%
    dplyr::summarize(tps = list(unlist(tps)), train = list(unlist(train)), phase = list(unlist(phaselab)), block_size = list(unlist(blocks)))

  #now put the trial and stimulus information back
  tb = tb %>% dplyr::rowwise() %>%
    dplyr::mutate(trials = list(trial_masterlist),
                  trial_names = list(trial_names_masterlist),
                  sdata[sdata$group == group, ])
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
  return(list(tps = tps, train = !tstps, block_size = block_size))
}

#function to return the gcd
.gcd <-  function(x,y) {
  r <- x%%y;
  return(ifelse(r, .gcd(y, r), y))
}


