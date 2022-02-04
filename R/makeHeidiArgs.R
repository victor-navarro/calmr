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

  #the only challenge here is to create a master list of trials (trials)
  #and sample the training for each group (ts)
  trials = unlist(unlist(lapply(design, function(g) lapply(g, function(p) p$trial_list)), recursive = F), recursive = F)
  trialnames_masterlist = unlist(unlist(lapply(design, function(g) lapply(g, function(p) p$trial_names)), recursive = F), recursive = F)
  #Reduce
  trials = trials[!duplicated(trialnames_masterlist)]
  trialnames_masterlist = trialnames_masterlist[!duplicated(trialnames_masterlist)]

  #heidi only requires the trial_masterlist, the trialname_masterlist is only used to determine the trial pointers that correspond to each group, given the masterlist
  #we can sample now

  #Dom, if you are reading this, I apologize for this bit
  #It basically creates a list of length iterations*groups*phases, for which each entry contains a vector of trial pointers
  ts = do.call(c, replicate(opts$iterations, lapply(design, function(g){
    unlist(lapply(g, function(p){
      ind = unlist(sapply(1:length(p$trial_names), function(t) rep(which(trialnames_masterlist %in% p$trial_names[t]), p$trial_repeats[t])))
      if (p$randomize){ind = ind[sample(length(ind))]} #This shuffling is safe from single trials ??sample
      return(ind)
    }), recursive = F)
  }), simplify = F)
  )

  #grab the group names
  groups = unlist(lapply(design, function(g) g[[1]]$group))
  snames = pars$Stimulus
  alphas = pars$Alpha
  cons = rep(1, length(alphas))
  names(alphas) = names(cons) = snames
  return(tibble::tibble(iteration = rep(1:opts$iterations, each = length(groups)),
                        group = rep(groups, opts$iterations),
                        trials = list(trials),
                        tps = ts,
                        trialnames = list(trialnames_masterlist),
                        stim_names = list(snames),
                        stim_alphas = list(alphas),
                        stim_cons = list(cons)))
}
