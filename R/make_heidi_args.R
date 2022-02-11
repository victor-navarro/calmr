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
#' make_heidi_args(design = des, pars = ps, opts = list(iterations = 1))
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
  #and sample the training for each group (ts)
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
    dplyr::mutate(tps = list(.sample_trial(trial_names, trial_repeats, randomize, trial_names_masterlist))) %>%
    dplyr::mutate(phaselab = list(rep(phase, length(tps)))) %>%
    #one last manipulation to concatenate phases into single rows
    dplyr::group_by(iteration, group) %>%
    dplyr::summarize(tps = list(unlist(tps)), phase = list(unlist(phaselab)))

  #now put the trial and stimulus information back
  tb = tb %>% dplyr::rowwise() %>%
    dplyr::mutate(trials = list(trial_masterlist),
                  trial_names = list(trial_names_masterlist),
                  sdata[sdata$group == group, ])
  tb
}

.sample_trial <- function(names, repeats, randomize, masterlist){
  #samples trials as a function of a masterlist, and randomizes if necessary
  tps = c()
  for (n in seq_along(names)){
    tps = c(tps, rep(which(masterlist %in% names[n]), repeats[n]))
  }
  if (randomize){
    tps = tps[sample(length(tps))]
  }
  tps
}
